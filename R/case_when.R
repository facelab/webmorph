#' case_when with JinT Evaluation
#'
#' Works like dplyr::case_when, but RHS doesn't all need to be the same data type, RHS isn't evaluated in LHS is FALSE, and LHE isn't evaluated unless all previous LHS are FALSE
#'
#' @param ... formulae to evaluate
#'
#' @return first true value or NULL
#' @export
#'
#' @examples
#' case_when_jit(F ~ "A", T ~ 1.1, bad_val ~ "B")
case_when_jit <- function(...) {
  env <- parent.frame() # not sure about this
  args <- match.call()

  if (length(args) == 1) return(NULL)

  for (i in 2:length(args)) {
    if (is.language(args[[i]])) {
      txt <- utils::capture.output(args[[i]]) %>%
        strsplit("\\s~\\s") %>% `[[`(1)
      test <- tryCatch(eval(parse(text = txt[[1]]),
                            envir = env))
      if (isTRUE(test)) {
        ret <- tryCatch(eval(parse(text = txt[[2]]),
                             envir = env))
        return(ret)
      }
    } else {
      warning("Argument ", (i-1), " is not a formula")
    }
  }

  NULL
}



#' Conditional Or
#'
#' @param a value (if not null)
#' @param b value if a is null
#'
#' @return a or (if a is null) b
#' @export
#'
#' @examples
#' x <- list(b = 2, c = 3)
#' x$a %||% x$b %||% x$c %||% "default_value"
#'
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


