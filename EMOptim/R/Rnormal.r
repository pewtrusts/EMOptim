#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"Rnorm" <- function(n, mean = 0, sd = 1) {
  if ((n <= 0) | is.na(n)) {
    return(0)
  } else {
    return(rnorm(n, mean, sd))
  }
}
