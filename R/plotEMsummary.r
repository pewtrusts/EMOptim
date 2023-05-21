#' Utility accessor function
#'
#' @author Alistair Dunn & Johanna Pierre.
#' @description Electronic Monitoring Video Review Rate Optimization. Pew Project: 2021-IF-02
#' @param data A summary iterate object from EMiterate
#' @param cost Plot the costs
#' @export
#'
plotEMsummary <- function(data, cost = TRUE) {
  plot(data$data$sampling.rate, data$data$cv, xlab = "Sampling rate", ylab = "Expected CV", type = "n", ylim = c(0, Max(c(unlist(data$data$cv) * 1.1, data$target.cv * 1.1))))
  title(paste0("Sampling efficiency for species=", data$species, " with fleet=", data$fleet, "\nusing encounter=", data$encounter))
  lines(data$data$sampling.rate, data$data$cv, type = "b", pch = 16)
  abline(h = data$target.cv, lty = 8, col = "gray")
  text(par()$usr[1] * 1.1, data$target.cv, "Target CV\n", adj = 0, col = "gray")
  x <- approx(data$data$cv, data$data$sampling.rate, xout = data$target.cv)
  abline(v = x$y, col = "lightseagreen")
  # plot cost
  if (cost) { 
    par(new = TRUE)
    plot(data$data$sampling.rate, data$data$cost.total, type = "n", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "blue", ylim = c(0, Max(data$data$cost.total) * 1.1))
    lines(data$data$sampling.rate, data$data$cost.total, col = "blue")
    lines(data$data$sampling.rate, data$data$cost.fleet, col = "lightblue", lty = 1)
    axis(side = 4, at = pretty(range(c(0, data$data$cost.total))), col = "blue", col.ticks = "blue", col.axis = "blue")
    mtext("Cost", side = 4, line = 3, col = "blue", las = 3)
  }
  invisible()
}
