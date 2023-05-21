#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"Rbinomial" <- function(n, p = 0) {
  if (is.na(n)) {
    return(NA)
  } else {
    return(rbinom(n, size = 1, p))
  }
}
