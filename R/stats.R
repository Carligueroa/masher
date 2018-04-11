#' Calculates a confidence interval and prints it nicely.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param ... further arguments to e passed to or from other methods.
ci <- function(x, ...) {
  int <- t.test(x, ...)

  if (int$estimate > 1e+3) {
    rnd <- 0
  } else if (int$estimate > 1e+2) {
    rnd <- 1
  } else {
    rnd <- 2
  }

  int <- paste(round(int$conf.int, rnd), collapse = ", ")
  int <- paste0("(", int, ")")

  return(int)
}
