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
      paste(collapse = "\n") %>% # in case it's an array
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
  projList <- projListGet() # sets $_SESSION['projects']
  if (nrow(projList) > 0) {
    projSet(projList$id[[1]])
  }

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

  message("You have ", resp$perm, " permissions for project ", project)

  Sys.setenv(webmorph_project_id = project)

  invisible(list(project_id = project,
                 permissions = resp$perm))
}

#' Get Project ID from a list of filenames
#'
#' @param files (leave blank to get current project ID)
#'
#' @return project ID
#' @export
#'
getProjectID <- function(files = NULL) {
  suppressWarnings({
    project_id <- gsub("^(\\d{1,11})/.*$", "\\1", files) %>%
     unique() %>% as.integer()
  })

  if (length(project_id) == 0 || is.na(project_id)) {
    # default project ID
    project_id <- Sys.getenv("webmorph_project_id")
  } else if (length(project_id) != 1) {
    stop("All files need to be in the same project.")
  }

  if (project_id == "") {
    stop("The project ID must be an intger and present at the start of each file name (e.g., '123/folder/file.jpg') or you can set the project with projSet(id)")
  }

  project_id
}


#' Get directory contents
#'
#' @param dir the directory to look in
#' @param project_id the project ID
#'
#' @return nested list of the directory
#' @export
#'
dirLoad <- function(dir = "", project_id = Sys.getenv("webmorph_project_id")) {
  resp <- web("dirLoad", subdir = paste(project_id, dir, sep = "/"),
              .error = "stop")

  # get all file paths
  purrr::flatten(resp$dir[[1]]) %>%
    `[`(. == "") %>%
    names() %>%
    sub(paste0("^i", project_id), "", .) # remove initial i (does stuff on the web)
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
  files <- sub("^/", paste0(getProjectID(files), "/"), files)

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
fileUpload <- function(files, dir = "/") {
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
  dir <- gsub("^/", paste0(project_id, "/"), dir)
  suppressMessages(check <- projSet(project_id))
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

  uploaded[uploaded != "FALSE"] %>%
    sub(paste0("^", project_id), "", .)
}

#' Delete directories on webmorph.org
#'
#' @param dir directory to delete
#'
#' @return logical, if directory was deleted
#' @export
dirDelete <- function(dir) {
  if (substr(dir, 0, 1) == "/") dir <- paste0(getProjectID(), dir)

  resp <- web("dirDelete", 'dirname[]' = dir)

  isTRUE(resp$info[dir] == "deleted")
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
  suppressMessages(check <- projSet(project_id))
  if (check$permissions != "all") {
    stop("You do not have permission to delete images from project ", check$project_id)
  }

  files <- sapply(files, gsub, pattern = "^/",
                  replacement = paste0(project_id, "/"))

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
#' @param outname local path to save average to
#' @param texture logical, textured average
#' @param norm how to normalise
#' @param normpoint points for twopoint normalisation
#' @param format image format
#'
#' @return webmorph_list
#' @export
#'
makeAvg <- function(files, outname = tempfile(),
                    texture = TRUE,
                    norm = c("none", "twopoint", "rigid"),
                    normpoint = 0:1,
                    format = c("jpg", "png", "gif")) {
  if ("webmorph_list" %in% class(files)) {
    # upload to temp dir first
    tdir <- sample(c(LETTERS, 0:9), 10) %>%
      paste(collapse = "") %>%
      paste0("/", ., "/")
    files <- fileUpload(files, tdir)
    # delete on exit
    on.exit(dirDelete(tdir))
  }

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

  dir.create(dirname(outname), recursive = TRUE, showWarnings = FALSE)
  imgname <- paste0(outname, ".", match.arg(format))
  temname <- paste0(outname, ".tem")
  file.copy(avg[[1]], imgname, overwrite = TRUE)
  file.copy(avg[[2]], temname, overwrite = TRUE)

  read_stim(c(imgname, temname))
}


#' Make a Transform
#'
#' The first 7 arguments are vectorised, so you can put in a vector of image names or shape/color/texture values.
#'
#' @param trans_img image to transform
#' @param from_img negative end of the transform dimension
#' @param to_img positive end of the transform dimension
#' @param shape amount to transform shape
#' @param color amount to transform color
#' @param texture amount to transform texture
#' @param outname local path to save transform to
#' @param norm how to normalise
#' @param normpoint points for twopoint normalisation
#' @param sample_contours whether to sample contours
#' @param warp warping algorithm to use
#' @param format image format
#'
#' @return webmorph_list
#' @export
#'
makeTrans <- function(trans_img = NULL, from_img = NULL, to_img = NULL,
                      shape = 0,
                      color = 0,
                      texture = 0,
                      outname = tempfile(),
                      norm = c("none", "twopoint", "rigid"),
                      normpoint = 0:1,
                      sample_contours = TRUE,
                      warp = c("multiscale", "linear", "multiscalerb"),
                      format = c("jpg", "png", "gif")) {
  # deal with webmorph lists
  if ("webmorph_list" %in% class(trans_img)) {
    # upload to temp dir first and # delete on exit
    tdir <- sample(c(LETTERS, 0:9), 10) %>% paste(collapse = "") %>% paste0("/", ., "/")
    trans_img <- fileUpload(trans_img, tdir)
    on.exit(dirDelete(tdir))
  }
  if ("webmorph_list" %in% class(from_img)) {
    tdir <- sample(c(LETTERS, 0:9), 10) %>% paste(collapse = "") %>% paste0("/", ., "/")
    from_img <- fileUpload(from_img, tdir)
    on.exit(dirDelete(tdir))
  }
  if ("webmorph_list" %in% class(to_img)) {
    tdir <- sample(c(LETTERS, 0:9), 10) %>% paste(collapse = "") %>% paste0("/", ., "/")
    to_img <- fileUpload(to_img, tdir)
    on.exit(dirDelete(tdir))
  }

  # set up a batch file
  files <- c(trans_img, from_img, to_img)
  project_id <- getProjectID(files)

  # select image files and remove project_id
  filenames <- list(trans = trans_img, from = from_img, to = to_img) %>%
    lapply(gsub, pattern = paste0("^", project_id), replacement = "") %>%
    lapply(function(x) { x[grepl("\\.(jpg|gif|png)$", x)] })

  # get all to the same length
  n_img <- sapply(filenames, length) %>% max()
  filenames <- lapply(filenames, rep_len, n_img) %>% as.data.frame()

  n_param <- list(shape, color, texture) %>%
    sapply(length) %>% max()
  param <- data.frame(
    shape = rep_len(shape, n_param),
    color = rep_len(color, n_param),
    texture = rep_len(texture, n_param)
  )

  batch <- tidyr::crossing(param, filenames)
  n <- nrow(batch)
  outname <- gsub("\\.(jpg|gif|png)$", "", outname)
  if (length(outname) < n) {
    outname <- rep_len(outname, n) %>%
      paste0("_", 1:n)
  }

  batch$outname <- outname[1:n]

  batchTrans(batch, project_id, norm, normpoint, sample_contours, warp, format)
}


#' Batch Transform
#'
#' @param batch data frame containing batch info
#' @param project_id the project ID
#' @param norm how to normalise
#' @param normpoint points for twopoint normalisation
#' @param sample_contours whether to sample contours
#' @param warp warping algorithm to use
#' @param format image format
#'
#' @return webmorph_list
#' @export
#'
batchTrans <- function(batch,
                       project_id = Sys.getenv("webmorph_project_id"),
                       norm = c("none", "twopoint", "rigid"),
                       normpoint = 0:1,
                       sample_contours = TRUE,
                       warp = c("multiscale", "linear", "multiscalerb"),
                       format = c("jpg", "png", "gif")) {
  # change columns like trans-img to trans
  nm <- names(batch)
  newnm <- gsub("-img$", "", nm)
  names(batch) <- newnm

  # check for required columns
  required <- c("trans", "from", "to", "shape", "color", "texture", "outname")
  missing <- setdiff(required, newnm)
  if (length(missing) > 0) {
    stop("The batch table is missing columns: ", paste(missing, collapse = ","))
  }

  # remove image suffixes and make local if starts with /
  batch$outname <- gsub("\\.(jpg|gif|png)$", "", batch$outname)
  batch$outname <- gsub("^/", "./", batch$outname)

  # clean parameters
  for (x in c("shape", "color", "texture")) {
    if (is.character(batch[[x]])) batch[x] <- gsub("%", "", batch[[x]]) %>% as.numeric()
    prob_pcnts <- abs(batch[[x]]) > 3
    batch[[x]][prob_pcnts] <- batch[[x]][prob_pcnts] / 100
  }

  transform(batch, project_id, norm, normpoint, sample_contours, warp, format)
}

#' Batch Transform  (internal)
#'
#' @param batch checked data frame containing batch info
#' @param project_id the project ID
#' @param norm how to normalise
#' @param normpoint points for twopoint normalisation
#' @param sample_contours whether to sample contours
#' @param warp warping algorithm to use
#' @param format image format
#'
#' @return webmorph_list

transform <- function(batch,
                      project_id = Sys.getenv("webmorph_project_id"),
                      norm = c("none", "twopoint", "rigid"),
                      normpoint = 0:1,
                      sample_contours = TRUE,
                      warp = c("multiscale", "linear", "multiscalerb"),
                      format = c("jpg", "png", "gif")) {

  tmpdir <- tempdir()
  imgname <- c()
  temname <- c()
  n <- nrow(batch)

  message("Starting ", n , " Transforms...")

  if (webmorph_options("verbose")) {
    pb <- progress::progress_bar$new(total = n)
    pb$tick(0)
  }

  for (i in 1:n) {
    query <- list(
      subfolder =  project_id,
      savefolder = '/.tmp/',
      count =  1,
      transimage0 = batch$trans[[i]],
      fromimage0 = batch$from[[i]],
      toimage0 = batch$to[[i]],
      shape0 = batch$shape[[i]],
      color0 = batch$color[[i]],
      texture0 = batch$texture[[i]],
      sampleContours0 = ifelse(isTRUE(as.logical(sample_contours)), "true", "false"),
      norm0 = match.arg(norm),
      warp0 = match.arg(warp),
      normPoint0_0 = normpoint[[1]],
      normPoint1_0 = normpoint[[2]],
      format0 = match.arg(format)
    )

    json_body <- jsonlite::toJSON(
      list(theData = query), auto_unbox = TRUE)

    url <- "https://webmorph.org/scripts/tcTransform"

    r <- httr::POST(url, body = json_body, encode = "raw")
    resp <- httr::content(r)
    if (isTRUE(resp$error)) { warning(resp$errorText) }

    suppressMessages(
      trans <- fileDownload(resp$newFileName, tmpdir)
    )

    dir.create(dirname(batch$outname[i]), recursive = TRUE, showWarnings = FALSE)
    imgname[i] <- paste0(batch$outname[i], ".", match.arg(format))
    temname[i] <- paste0(batch$outname[i], ".tem")
    file.copy(trans[[1]], imgname[[i]], overwrite = TRUE)
    file.copy(trans[[2]], temname[[i]], overwrite = TRUE)
    pb$tick()
  }

  message("... Transforms Completed")

  read_stim(c(imgname, temname))
}


# auto_delineate <- function(files) {
#   if ("webmorph_list" %in% class(files)) {
#     stimlist <- files
#     # upload to temp dir first
#     tdir <- sample(c(LETTERS, 0:9), 10) %>%
#       paste(collapse = "") %>%
#       paste0("/", ., "/")
#     files <- fileUpload(files, tdir)
#     # delete on exit
#     on.exit(dirDelete(tdir))
#   }
#
#   x <- lapply(files, web, script = "temFaceplusplus")
# }
