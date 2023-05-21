#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"Runif" <- function(n, min = 0, max = 1) {
  if ((n <= 0) | is.na(n)) {
    return(0)
  } else {
    return(runif(n, min, max))
  }
}
