#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
Weighted.sd <- function(x, w) {
  xv <- Weighted.mean(x^2, w)
  res <- sqrt(xv)
  res[!is.finite(res)] <- 0
  return(res)
}
