#' Utility accessor function
#'
#' @author Alistair Dunn & Johanna Pierre.
#' @description Electronic Monitoring Video Review Rate Optimization. Pew Project: 2021-IF-02
#' @param type Type of map (base or command)
#' @param label the command label
#' @param plotit either plot the data or return the object
#' @param as.image if true, convert character vectors to numeric, and plot as an image (default  = FALSE)
#' @param zero.rm Remove zeros and replace with NAs (default = TRUE)
#' @param presence.only Plot presense/absence (i.e., values > 0) only (default = FALSE)
#' @param ... other arguments supplied to plot()
#' @export
#'
plotEMmap <- function(EM, type, label, plotit = TRUE, as.image = FALSE, zero.rm = TRUE, presence.only = FALSE, ...) {
  base <- t(EM[["base"]]$table$data)
  base <- base[, ncol(base):1]
  base[base <= 0] <- NA
  if (type == "base") {
    data <- EM[["base"]]$table$data
  } else {
    data <- EM[[type]][[label]]$table$data
  }
  data <- t(data)
  data <- data[, ncol(data):1]
  if (zero.rm) {
    data[data <= 0] <- NA
  }
  if(presence.only) {
    data[data > 0] <- 1
  }
  if (plotit & type != "strata") {
    image(x = 1:nrow(base), y = 1:ncol(base), z = base, axes = FALSE, col=c("grey80"), ...)
    image(x = 1:nrow(data), y = 1:ncol(data), z = data, axes = FALSE, add= TRUE)
    box()
  }
  if (plotit & type == "strata") {
    if (!as.image) {
      image(x = 1:nrow(base), y = 1:ncol(base), z = base, axes = FALSE, col=c("grey80"), ...)
      image(x = 1:nrow(data), y = 1:ncol(data), z = matrix(NA, ncol = ncol(data), nrow = nrow(data)), axes = FALSE, add = TRUE)
      data[data == "NA"] <- ""
      text(row(data), col(data), data)
    } else {
      z <- as.numeric(as.factor(data))
      image(x = 1:nrow(data), y = 1:ncol(data), z = data, axes = FALSE, ...)
    }
    box()
  }
  axis(side=1, labels = FALSE)
  axis(side=2, labels = FALSE)
  invisible(data)
}
