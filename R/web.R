#' Webmorph.org API
#'
#' @param script the name of the webmorph script
#' @param ... arguments to pass on to the script in POST
#' @param .error whether to warn, stop or do nothing on error
#'
#' @return the response as a list
#' @export
#'
#' @examples
#'
#' web(a = 1, b = FALSE, c = "testing")
web <- function(script = "webmorphR", ..., .error = c("warn", "stop", "none")) {list(...)
  url <- paste0("https://webmorph.org/scripts/", script)
  r <- httr::POST(url, body = list(...))
  resp <- httr::content(r)
  if (isTRUE(resp$error)) {
    .error <- match.arg(.error)
    e <- resp$errorText %>%
      charToRaw() %>%  # force read_html to treat as string
      xml2::read_html() %>% # read as HTML
      rvest::html_text() # strip HTML tags
    if (.error == "warn") warning(e, call. = FALSE)
    if (.error == "stop") stop(e, call. = FALSE)
  }
  invisible(resp)
}

#' Login to webmorph.org
#'
#' @param email The email address associated with the account
#' @param password The password for the account
#'
#' @return NULL
#' @export
login <- function(email = Sys.getenv("WEBMORPH_EMAIL"),
                  password = Sys.getenv("WEBMORPH_PASSWORD")) {
  resp <- web("userLogin", email = email, password = password, .error = "stop")
  message("Logged in as user ", resp$user)
  projListGet() # sets $_SESSION['projects']

  invisible(resp$user)
}

#' Log out of webmorph.org
#'
#' @return NULL
#' @export
logout <- function() {
  resp <- web("userLogout")
  if (isFALSE(resp$error)) message("Logged out")
}

#' Get webmorph.org project list
#'
#' @param notes whether to resturn the notes for each project
#'
#' @return data frame of project id, name and notes (optional)
#' @export
#'
projListGet <- function(notes = FALSE) {
  resp <- web("projListGet", .error = "stop")

  msg <- sprintf("Your projects are using %s of %s",
          utils:::format.object_size(resp$userAllocation$size*1024*1024, "auto"),
          utils:::format.object_size(resp$userAllocation$allocation*1024*1024, "auto")
  )
  message(msg)

  proj <- data.frame(
    id = sapply(resp$projects, `[[`, "id"),
    name = sapply(resp$projects, `[[`, "name")
  )

  if (notes) proj$notes = sapply(resp$projects, `[[`, "notes")

  proj
}

#' Set the Working Project
#'
#' @param project the ID of the project to set
#'
#' @return project id and permissions
#' @export
#'
projSet <- function(project) {
  resp <- web("projSet", project = project, .error = "stop")

  invisible(list(project_id = project,
                 permissions = resp$perm))
}

#' Get Project ID from a list of filenames
#'
#' @param files
#'
#' @return project ID
#' @export
#'
getProjectID <- function(files) {
  suppressWarnings({
    project_id <- gsub("^(\\d{1,11})/.*$", "\\1", files) %>%
     unique() %>% as.integer()
  })

  if (length(project_id) != 1) {
    stop("All files need to be in the same project.")
  } else if (is.na(project_id)) {
    stop("The project ID must be an intger and present at the start of each file name (e.g., '123/folder/file.jpg')")
  }

  project_id
}


#' Get directory contents
#'
#' @param project_id the project ID
#' @param dir the directory to look in
#'
#' @return nested list of the directory
#' @export
#'
dirLoad <- function(project_id, dir = "") {
  resp <- web("dirLoad", subdir = paste(project_id, dir, sep = "/"),
              .error = "stop")

  # get all file paths
  purrr::flatten(resp$dir[[1]]) %>%
    `[`(. == "") %>%
    names() %>%
    sub("^i", "", .) # remove initial i (does stuff on the web)
}

#' Download files from webmorph
#'
#' @param files a list or vector of file names to download, must start with the project number, e.g. "1/averages/f_multi.jpg"
#' @param destination A folder to save the files to, defaults to the directory structure of the project
#'
#' @return a list of local paths to the files
#' @export
#'
fileDownload <- function(files, destination = NULL) {
  if (length(files) > 1) {
    paths <- sapply(files, fileDownload, destination = destination)
    # names(paths) <- NULL
    message("Downloaded ", length(paths), " files")
    return(invisible(paths))
  }

  if (is.null(destination)) {
    fname <- files
  } else {
    fname <- file.path(destination, basename(files))
  }
  dir.create(dirname(fname), recursive = TRUE, showWarnings = FALSE)

  r <- httr::POST("https://webmorph.org/scripts/fileZip",
                  body = list(files = files),
                  httr::write_disk(fname, TRUE))

  invisible(fname)
}

#' Upload files to webmorph.org
#'
#' @param files vector of paths of files to upload
#' @param dir directory to upload them to
#'
#' @return list of successfully uploaded files
#' @export
#'
fileUpload <- function(files, dir) {
  url <- "https://webmorph.org/scripts/fileUpload"

  # save  images to tempdir if files is a stim list
  if ("webmorph_list" %in% class(files)) {
    stimlist <- files
    files <- write_stim(stimlist, tempdir()) %>%
      unlist()

    names(files) <- sapply(files, basename)
  }

  # need to set current project to avoid permissions rejection
  dir <- paste0(dir, "/") %>% gsub("/+", "/", .)
  project_id <- getProjectID(dir)
  check <- projSet(project_id)
  if (check$permissions != "all") {
    stop("You do not have permission to upload images to project ", check$project_id)
  }

  message("Starting Upload...")

  if (webmorph_options("verbose")) {
    pb <- progress::progress_bar$new(total = length(files))
    pb$tick(0)
  }

  uploaded <- sapply(files, function(path) {
    body <- list(`upload[0]` = httr::upload_file(path),
                 basedir = dir)
    r <- httr::POST(url, body = body)
    resp <- httr::content(r)
    if (webmorph_options("verbose")) pb$tick()
    if (isTRUE(resp$error)) {
      warning(resp$errorText)
      FALSE
    } else {
      resp$newFileName[[1]]
    }
  })

  message("... ", sum(uploaded != "FALSE"), " of ",
          length(files), " uploaded")

  uploaded[uploaded != "FALSE"]
}

#' Delete directories on webmorph.org
#'
#' @param dir directory to delete
#'
#' @return logical, if directory was deleted
#' @export
dirDelete <- function(dir) {
  resp <- web("dirDelete", 'dirname[]' = dir)

  resp$info[dir] == "deleted"
}

#' Delete files on webmorph.org
#'
#' @param files files to delete
#'
#' @return list of deleted files
#' @export
fileDelete <- function(files) {
  # need to set current project to avoid permissions rejection
  project_id <- getProjectID(files)
  check <- projSet(project_id)
  if (check$permissions != "all") {
    stop("You do not have permission to delete images from project ", check$project_id)
  }

  message("Starting Deletion...")

  if (webmorph_options("verbose")) {
    pb <- progress::progress_bar$new(total = length(files))
    pb$tick(0)
  }

  #names(files) <- rep('files[]', length(files))
  #do.call(web, c(list(script = "fileDelete"), files))

  deleted <- sapply(files, function(path) {
    resp <- web("fileDelete", 'files[]' = path)
    if (webmorph_options("verbose")) pb$tick()
    if (isTRUE(resp$error)) {
      warning(resp$errorText)
      FALSE
    } else {
      TRUE
    }
  })

  message("... ", sum(deleted), " of ",
          length(deleted), " deleted")

  deleted
}

#' Make an Average Face
#'
#' @param files the image files to average
#' @param savedir what directory to save the average in
#' @param texture logical, textured average
#' @param norm how to normalise
#' @param normpoint points for twopoint normalisation
#' @param format image format
#'
#' @return webmorph_list
#' @export
#'
makeAvg <- function(files, filename = "avg",
                    texture = TRUE,
                    norm = c("none", "twopoint", "rigid"),
                    normpoint = 0:1,
                    format = c("jpg", "png", "gif")) {

  project_id <- getProjectID(files)

  # select image files and remove project_id
  filenames <- gsub(paste0("^", project_id), "", files)
  filenames <- filenames[grepl("\\.(jpg|gif|png)$", files)]

  query <- list(
    subfolder =  project_id,
    savefolder = '/.tmp/',
    count =  1,
    texture0 = ifelse(isTRUE(as.logical(texture)), "true", "false"),
    norm0 = match.arg(norm),
    normPoint0_0 = normpoint[[1]],
    normPoint1_0 = normpoint[[2]],
    format0 = match.arg(format),
    images0 = filenames
  )

  json_body <- jsonlite::toJSON(
    list(theData = query), auto_unbox = TRUE)

  url <- "https://webmorph.org/scripts/tcAverage"

  r <- httr::POST(url, body = json_body, encode = "raw")
  resp <- httr::content(r)
  if (isTRUE(resp$error)) { warning(resp$errorText) }

  tmpdir <- tempdir()
  suppressMessages(
    avg <- fileDownload(resp$newFileName, tmpdir)
  )

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  imgname <- paste0(filename, ".", match.arg(format))
  temname <- paste0(filename, ".tem")
  file.copy(avg[[1]], imgname, overwrite = TRUE)
  file.copy(avg[[2]], temname, overwrite = TRUE)

  read_stim(c(imgname, temname))
}

