#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @keywords internal
#'
"get.table.subcommands" <- function() {
  command <- c("table", "end_table")
  type <- c("table_label", "end_table")
  res <- list(command = command, type = type)
  return(res)
}
