#' EM utility function
#'
#' This function summarises an EMiterate output into a simple table for viewing and plotting
#'
#' @author Alistair Dunn & Johanna Pierre
#' @param EM An EM object
#' @param EMiterations the output of EMiternations
#' @param stratum Optionally, output the results from a specified stratum within strata.label
#' @export
#'
EMoptimise <- function(EM, EMiterations, sampling.fractions = NULL, stratum = NULL) {
  objective.label <- EMiterations[[1]]$objective.label
  if (!(objective.label %in% names(EM$objective))) {
    stop("The @objective with label ", objective.label, " was not found in the config file")
  }
  objective <- EM$objective[[objective.label]]
  cv <- objective$cv
  strata.label <- EMiterations[[1]]$strata
  # Summarise
  result <- EMsummary(EM = EM, EMiterations = EMiterations, stratum = stratum)
  # estimate optimal sampling.rate
  optimal.sampling <- approx(result$data$cv, result$data$sampling.rate, xout = cv, rule = 2)$y
  cat(paste("Optimal sampling =",optimal.sampling,"\n"))
  # re-run calculation with this sampling.rate
  result <- EMsample(EM, objective.label, strata.label, N = NULL, sampling.rate = optimal.sampling, nsims = NULL, quiet = TRUE, specification.only = FALSE, keep.samples = FALSE, sampling.fractions = sampling.fractions)
  return(result)
}
