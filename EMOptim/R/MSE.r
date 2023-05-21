#' EM utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' internal
#'
"mse" <- function(estimate, actual) {
  res <- sqrt(Mean((estimate - actual)^2))
  res <- res / Mean(actual)
  return(res)
}
