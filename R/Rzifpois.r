#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"Rzifpois" <- function(n, lambda, p) {
  if ((n <= 0) | is.na(n)) {
    return(0)
  } else {
    res1 <- rbinom(n, size = 1, p)
    res2 <- rpois(n, lambda)
    return((1 - res1) * res2) 
  }
}
