#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
Length <- function(x) {
  length(x[!is.na(x)])
}
