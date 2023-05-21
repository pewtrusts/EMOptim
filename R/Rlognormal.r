#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"Rlognorm" <- function(n, mean, cv) {
  if ((n <= 0) | is.na(n)) {
    return(0)
  } else if (!(mean > 0)) {
    return(rep(NA, n))
  } else {
    logvar <- sqrt(log(cv^2 + 1))
    logmean <- log(mean) - (logvar^2) / 2
    return(exp(rnorm(n, logmean, logvar)))
  }
}
