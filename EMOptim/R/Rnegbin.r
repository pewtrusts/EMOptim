#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"Rnegbin" <- function(n, mu = n, theta = stop("'theta' must be specified")) {
  k <- if (length(n) > 1) {
    length(n)
  } else {
    n
  }
  return(Rpois(k, (mu * rgamma(k, theta)) / theta))
}
