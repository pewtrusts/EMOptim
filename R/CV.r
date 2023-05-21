#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
CV <- function(x) {
  return(sqrt(var(x, na.rm = TRUE)) / Mean(x))
}
