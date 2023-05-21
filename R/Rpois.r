#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"Rpois" <- function(n, lambda) {
  if ((n <= 0) | is.na(n)) {
    return(0)
  } else {
    return(rpois(n, lambda))
  }
}
