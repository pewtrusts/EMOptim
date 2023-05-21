#' Utility accessor function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @param EM An EM object
#' @param object Character string with the command. If missing, returns a list of the commands in EM
#' @param string Character string with the command label. If missing, returns a list of labels of 'object' in EM
#' @export
#
"getObject" <- function(EM, object, string) {
  if (missing(object)) {
    res <- names(EM)
  } else if (missing(string)) {
    res <- names(EM[[object]])
  } else {
    res <- EM[[object]][[string]]
  }
  return(res)
}
