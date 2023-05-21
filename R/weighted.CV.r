#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
Weighted.CV <- function(x, w) {
  xm <- Weighted.mean(x, w)
  var <- Sum(w * (x - xm)^2)
  return(sqrt(var) / xm)
}
