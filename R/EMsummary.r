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
EMsummary <- function(EM, EMiterations, stratum = NULL) {
  objective <- EMiterations[[1]]$objective
  species <- EMiterations[[1]]$species
  fleet <- EMiterations[[1]]$fleet
  encounter <- EMiterations[[1]]$encounter
  sampling.rate <- sapply(EMiterations, function(x) {
    return(x$sampling.rate)
  })
  cost.total <- sapply(EMiterations, function(x) {
    return(x$cost$total)
  })
  cost.fleet <- sapply(EMiterations, function(x) {
    return(x$cost$fleet)
  })
  cost.species <- sapply(EMiterations, function(x) {
    return(x$cost$species)
  })
  res <- list("objective" = objective, "species" = species, "fleet" = fleet, "encounter" = encounter)
  cv <- c()
  N <- c()
  if (is.null(stratum)) {
    for (i in 1:length(EMiterations)) {
      cv[i] <- EMiterations[[i]]$cv
      N[i] <- EMiterations[[i]]$N
    }
  } else {
    if (!stratum %in% EMiterations[[1]]$parameters$strata) {
      stop(paste0("stratum value '", stratum, "' is not a valid stratum label in strata '", EMiterations[[1]]$strata, "'. Valid stratum's are: ", paste(EMiterations[[1]]$parameters$strata, collapse = ", ")))
    }
    cv <- sapply(EMiterations, function(x) {
      return(x$strata.sampling.cv[x$parameters$strata == stratum])
    })
    N <- sapply(EMiterations, function(x) {
      return(x$parameters$N[x$parameters$strata == stratum])
    })
  }
  data <- list()
  data$strata <- sapply(EMiterations, function(x) {
    return(x$strata)
  })
  if (!is.null(stratum)) {
    data$stratum <- rep(stratum, length(data$strata))
  }
  data$sampling.rate <- sampling.rate
  data$N <- N
  data$cost.total <- cost.total
  data$cost.fleet <- cost.fleet
  data$cost.species <- cost.species
  data$cv <- cv
  res$target.cv <- EM$objective[[objective]]$cv
  res$data <- do.call(cbind.data.frame, data)
  return(res)
}
