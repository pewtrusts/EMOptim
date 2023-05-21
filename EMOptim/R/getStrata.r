#' Utility accessor function
#'
#' @author Alistair Dunn & Johanna Pierre
#'
#' @export
#'
getStrata <- function(object, strata.label = NULL) {
  if (is.null(strata.label)) {
    res <- object[["strata"]]
    return(names(res))
  } else {
    res <- object[["strata"]][[strata.label]]$table$data
    return(unique(res[res != "NA"]))
  }
}
