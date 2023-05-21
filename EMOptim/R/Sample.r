#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"Sample" <- function(x, size, replace = FALSE, prob = NULL) {
  return(sample(x, size, replace = replace, prob = prob))
}
