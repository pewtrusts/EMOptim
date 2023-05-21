#' EM utility function
#'
#' This function simulates data
#'
#' @author Alistair Dunn & Johanna Pierre
#' @param EM an EM object
#' @param objective.label The label of the objective function to simulate and sample from
#' @param quiet If !quiet, then print information messages during the sampling
#' @param sampling.fractions Provides the sampling fractions for each strata. If NULL (the default), then determined using Neyman allocation
#' @export
#'
EMiterate <- function(EM, objective.label, strata.label, sampling.fractions = NULL, quiet = TRUE, parallel = TRUE, cores = NULL) {
  reregister_dopar <- function(CORES = 0) {
    # fix for "Error in summary.connection(connection) : invalid connection"
    env <- foreach:::.foreachGlobals
    rm(list = ls(name = env), pos = env)
    if (CORES < 1) {
      CORES <- floor(parallel::detectCores() / 2)
    }
    doParallel::registerDoParallel(CORES)
    return(CORES)
  }
  if (parallel == FALSE) {
    cat("Optimisation using parallel = FALSE\n")
    cores <- 1
  } else {
    if (is.null(cores)) {
      cores <- reregister_dopar()
    } else {
      cores <- reregister_dopar(cores)
    }
    cat("Optimisation using parallel = TRUE. Using", cores, "cores\n")
  }

  objective <- EM$objective[[objective.label]]
  objective.type <- objective$type
  sampling.rate <- EM$simulations$sampling_rate
  min.samples <- EM$simulations$min_sampling_rate
  max.samples <- EM$simulations$max_sampling_rate
  if (min.samples >= max.samples) {
    stop(paste0("The min_sampling_rate (", min.samples, ") cannot be larger than or equal to max_sampling_rate (", max.samples, ")"))
  }
  if ((sampling.rate < min.samples) | (sampling.rate > max.samples)) {
    stop(paste0("The sampling_rate (", sampling.rate, ") must be between the min_sampling_rate (", max.samples, ") and the max_sampling_rate (", max.samples, ")"))
  }
  steps <- EM$simulations$steps
  if (steps < 1) {
    stop(paste0("The step_size (", steps, ") cannot be less than one"))
  }
  nsamples <- seq(from = min.samples, to = max.samples, length.out = steps)
  if (!quiet) {
    cat(paste0("Simulating ", EM$simulations$n_simulations, " over rates = ", paste(nsamples, collapse = " "), "\n"))
  }
  if (exists("fraction", where = EM$strata[[strata.label]])) {
    sampling.fractions <- EM$strata[[strata.label]]$fraction$fraction
  } else {
    sampling.fractions <- NULL
  }
  res <- list()
  if (parallel) {
    res <- foreach(i = 1:length(nsamples), .packages = "EMoptim") %dopar% {
      if (!quiet) cat(paste0("Simulating for rate = ", nsamples[i], "\n"))
      EMsample(EM = EM, objective.label = objective.label, strata.label = strata.label, sampling.rate = nsamples[i], sampling.fractions = sampling.fractions)
    }
    for (i in 1:length(nsamples)) {
      res[[i]]$objective.label <- objective.label
    }
    stopImplicitCluster()
  } else {
    for (i in 1:length(nsamples)) {
      if (!quiet) cat(paste0("Simulating for rate = ", nsamples[i], "\n"))
      res[[i]] <- EMsample(EM = EM, objective.label = objective.label, strata.label = strata.label, sampling.rate = nsamples[i], sampling.fractions = sampling.fractions)
      res[[i]]$objective.label <- objective.label
    }
  }
  names(res) <- 1:length(res)
  return(res)
}
