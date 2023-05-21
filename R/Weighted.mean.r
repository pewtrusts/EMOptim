#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
Weighted.mean <- function(x, w, ...) {
  res <- weighted.mean(x, w, ..., na.rm = TRUE)
  res[!is.finite(res)] <- 0
  return(res)
}
