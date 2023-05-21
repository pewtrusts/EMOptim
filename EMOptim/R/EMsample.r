#' EM utility function
#'
#' This function does most of the work .. samples for a specified objective
#'
#' @author Alistair Dunn & Johanna Pierre
#' @param objective.label The label of the objective function to simulate and sample from
#' @param strata.label The label of the stratification definition to use
#' @param N The number of samples. If NULL, then defaults to the sampling rate to calculate the number of samples
#' @param sampling.rate The sampling rate (a proportion between 0 and 1). See N
#' @param nsims The number of simulations to run. Defaults to the number defined in the EM object
#' @param quiet If false, then print information messages during the sampling
#' @param specification.only if true, then only return the objective specification parameters and the cell specific mu's and sd's
#' @param keep.samples if true, then returns the individual simulation sample means and sd's
#' @param sampling.fractions Provides the sampling fractions for each strata. If NULL (the default), then determined using Neyman allocation
#' @export
#'
EMsample <- function(EM, objective.label, strata.label, N = NULL, sampling.rate = NULL, nsims = NULL, quiet = TRUE,
                     specification.only = FALSE, keep.samples = FALSE, sampling.fractions = NULL) {
  if (is.null(nsims)) {
    nsims <- EM$simulations$n
  } 
  if (nsims <= 0) {
    stop("nsims must be specified and be greater then 0")
  }
  if (!quiet) cat(paste0("nsims = ", nsims, "\n"))

  if (is.null(sampling.rate)) {
    sampling.rate <- EM$simulations$sampling_rate
  }
  if (is.null(N) & (sampling.rate <= 0 | sampling.rate >= 1)) {
    stop("@simulations$sampling_rate must be specified, and between 0 and 1")
  }
  if (!quiet) cat(paste0("Default sampling.rate = ", sampling.rate, "\n"))

  if (!(objective.label %in% names(EM$objective))) {
    stop("The @objective with label ", objective.label, " was not found in the config file")
  }
  if (!quiet) cat(paste0("objective.label = ", objective.label, "\n"))
  objective <- EM$objective[[objective.label]]
  objective.type <- objective$type
  if (exists("min_value", where = objective)) {
    min.value <- objective$min_value
  } else {
    min.value <- 0
  }

  encounter.label <- objective$encounter
  if (!quiet) cat(paste0("encounter.label = ", encounter.label, "\n"))
  encounter.labels <- names(EM$encounter)
  if (!(encounter.label %in% encounter.labels)) {
    stop(paste0("The @encounter with label ", encounter.label, " does not exist as an encounter definition"))
  }
  encounter <- EM$encounter[[encounter.label]]

  species.label <- EM$encounter[[encounter.label]]$species
  if (!quiet) cat(paste0("species.label = ", species.label, "\n"))
  species.labels <-  names(EM$species)
  if (!(species.label %in% species.labels)) {
    stop(paste0("The @species with label ", species.label, " does not exist as a species definition"))
  }
  species <- EM$species[[species.label]]$table$data
  species.cost <- EM$species[[species.label]]$cost
  species[species < min.value] <- min.value

  fleet.label <- EM$encounter[[encounter.label]]$fleet
  if (!quiet) cat(paste0("fleet.label = ", fleet.label, "\n"))
  fleet.labels <-  names(EM$fleet)
  if (!(fleet.label %in% fleet.labels)) {
    stop(paste0("The @fleet with label ", fleet.label, " does not exist as a fleet definition"))
  }
  fleet <- EM$fleet[[fleet.label]]$table$data
  fleet.cost <- EM$fleet[[fleet.label]]$cost

  if (!specification.only) {
    if (!(strata.label %in% names(EM$strata))) {
      stop("The @strata with label ", strata.label, " was not found in the config file")
    }
    if (!quiet) cat(paste0("strata.label = ", strata.label, "\n"))
    strata <- EM$strata[[strata.label]]$table$data
  }

  if (!exists("cost_function", objective)) {
    objective$cost_function <- "standard"
  } # Only standard implemented
  cost <- cost_function(
    type = objective$cost_function, N_fleet = Sum(sapply(EM$fleet[[fleet.label]]$table, Sum)), fleet = fleet.cost,
    N_species = Sum(sapply(EM$fleet[[fleet.label]]$table, Sum) * sampling.rate), species = species.cost
  )

  # calculate distribution expected abundance, mu = random_distribution * species_distribution, + calcuate sd's
  if (casefold(encounter$type) %in% "binomial") {
    mu <- encounter$p * as.vector(species)
    sd <- sqrt(mu * (1 - mu))
  } else if (casefold(encounter$type) %in% "lognormal") {
    mu <- encounter$mu * as.vector(species)
    sd <- encounter$cv * mu
  } else if (casefold(encounter$type) %in% "negative_binomial") {
    mu <- encounter$mu * as.vector(species)
    sd <- sqrt(mu + (mu^2 / encounter$theta))
  } else if (casefold(encounter$type) %in% "normal") {
    mu <- encounter$mu * as.vector(species)
    sd <- rep(encounter$sd, length(mu))
  } else if (casefold(encounter$type) %in% "poisson") {
    mu <- encounter$lambda * as.vector(species)
    sd <- mu
  } else if (casefold(encounter$type) %in% "zifpoisson") {
    mu <- (1 - encounter$p) * encounter$lambda * as.vector(species) 
    sd <- mu * (1 - encounter$p) * (1 + encounter$p * mu)
  } else if (casefold(encounter$type) %in% "uniform") {
    mu <- (encounter$min + encounter$max) / 2 * as.vector(species)
    sd <- (encounter$max - encounter$min) / (2 * sqrt(3))
  } else {
    stop("The specified distribution is not known")
  }

  # Determine total N and Ni's from sampling rate unless specified above
  if (is.null(N)) {
    Ni <- fleet * sampling.rate
    N <- Sum(Ni)
  } else {
    Ni <- N * fleet / Sum(fleet)
    sampling.rate <- N / Sum(fleet)
  }

  if (!specification.only) {
    # Set up data frame to record the strata level data
    Nstrata <- length(unique(as.vector(strata[!strata %in% c(NA, "NA", "")])))
    res <- data.frame(
      "strata" = unique(as.vector(strata[!strata %in% c(NA, "NA", "")])), "N.population" = rep(NA, Nstrata),
      fraction = rep(NA, Nstrata), "mu" = rep(NA, Nstrata), "sd" = rep(NA, Nstrata), "N" = rep(NA, Nstrata)
    )
    for (i in 1:length(res$strata)) {
      index <- !is.na(as.vector(strata)) & as.vector(strata) == res$strata[i]
      res$N.population[i] <- Sum(fleet[index])
      res$mu[i] <- Weighted.mean(x = mu[index], w = Ni[index])
      res$sd[i] <- Weighted.sd(x = sd[index], w = Ni[index])
    }
    # Calculate sampling fraction and then allocate N to strata using Neyman allocation
    if (is.null(sampling.fractions)) {
      res$fraction <- res$N.population * res$sd / Sum(res$N.population * res$sd)
      res$fraction[!is.finite(res$fraction)] <- 0
      res$N <- round(res$fraction * N) # round to ensure integers
    } else {
      res$fraction <- sampling.fractions / Sum(sampling.fractions)
      res$N <- round(res$fraction * N) # round to ensure integers
    }
  } else {
    res <- data.frame("id" = 1:length(mu), "N.population" = as.vector(fleet), fraction = rep(NA, length(mu)), "mu" = mu, "sd" = sd, "N" = rep(NA, length(mu)))
    if (is.null(sampling.fractions)) {
      res$fraction <- res$N.population * res$sd / Sum(res$N.population * res$sd)
      res$N <- round(res$fraction * N) # round to ensure integers
    } else {
      res$N <- round(sampling.fractions * N) # round to ensure integers
    }
    return(list(
      "objective" = objective.label, "species" = species.label, "fleet" = fleet.label, "encounter" = encounter.label,
      "cost" = cost, "sampling.rate" = sampling.rate, "N" = N, "parameters" = res
    ))
  }

  # Now simulate to get expected sample sd's for given N, using appropriate distribution
  sample.mu <- matrix(NA, ncol = length(res$strata), nrow = nsims)
  colnames(sample.mu) <- res$strata
  sample.sd <- matrix(NA, ncol = length(res$strata), nrow = nsims)
  colnames(sample.sd) <- res$strata

  sampling.cv <- matrix(NA, ncol = length(res$strata), nrow = 1)
  colnames(sampling.cv) <- res$strata

  # iterate over strata
  for (i in 1:length(res$strata)) {
    index <- !is.na(as.vector(strata)) & as.vector(strata) == res$strata[i]
    index <- (1:length(index))[index]
    for (k in 1:nsims) {
      samples <- c()
      # iterate over cells within strata
      for (j in 1:length(index)) {
        n <- res$N[i] * (fleet[index[j]] / res$N.population[i])
        if (casefold(encounter$type) %in% "binomial") {
          samples <- c(samples, Rbinomial(n = n, size = 1, p = encounter$p * species[index[j]]))
        } else if (casefold(encounter$type) %in% "lognormal") {
          samples <- c(samples, Rlognorm(n = n, mean = encounter$mu * species[index[j]], cv = encounter$cv))
        } else if (casefold(encounter$type) %in% "negative_binomial") {
          samples <- c(samples, Rnegbin(n = n, mu = encounter$mu * species[index[j]], theta = encounter$theta))
        } else if (casefold(encounter$type) %in% "normal") {
          samples <- c(samples, Rnorm(n = n, mean = encounter$mu * species[index[j]], sd = encounter$sd))
        } else if (casefold(encounter$type) %in% "poisson") {
          samples <- c(samples, Rpois(n = n, lambda = encounter$lambda * species[index[j]]))
        } else if (casefold(encounter$type) %in% "zifpoisson") {
          samples <- c(samples, Rzifpois(n = n, lambda = encounter$lambda * species[index[j]], p = encounter$p))
        } else if (casefold(encounter$type) %in% "uniform") {
          samples <- c(samples, Runif(n = n, min = min * species[j], max = max * species[index[j]]))
        } else {
          stop(paste("encounter type =", encounter.type, "is not a valid encounter.type"))
        }
      }
      sample.mu[k, i] <- Mean(samples)
      sample.sd[k, i] <- sd(samples, na.rm = TRUE)
    }
    sampling.cv[i] <- mse(sample.mu[, i], res$mu[i])
  }
  # overall cv
  overall.sampling.mu <- apply(sample.mu, 1, Weighted.mean, w = res$N)
  overall.cv <- mse(overall.sampling.mu, Weighted.mean(res$mu, res$N))

  output <- list(
    "objective" = objective.label, "species" = species.label, "fleet" = fleet.label, "encounter" = encounter.label,
    "strata" = strata.label, "cost" = cost, "sampling.rate" = sampling.rate, "N" = N,
    "parameters" = res, "strata.sampling.cv" = sampling.cv, "cv" = overall.cv
  )

  if (keep.samples) {
    output[["sample.mu"]] <- sample.mu
    output[["sample.sd"]] <- sample.sd
  }
  return(output)
}
