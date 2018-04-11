#' Calculates a confidence interval and prints it nicely.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param ... further arguments to e passed to or from other methods.
ci <- function(x, ...) {
  int <- t.test(x, ...)
  int <- paste(round(int$conf.int, 2), collapse = ", ")
  int <- paste0("(", int, ")")

  return(int)
}
