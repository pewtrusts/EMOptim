#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"Rgamma" <- function(n, shape, rate = 1, scale = 1 / rate) {
  if ((n <= 0) | is.na(n)) {
    return(0)
  } else {
    return(rgammaf(n, shape, rate = 1, scale = 1 / rate))
  }
}
