#' EM utility function
#'
#' This function uses the SamplingStrata package to find optimal strata for multiple objectives
#'
#' @author Alistair Dunn & Johanna Pierre
#' @param EM an EM object
#' @param objective.labels The labels of the objective function to simulate and sample from (must be a vector of length > 1)
#' @param new.strata.label The label for the optimised strata calculated by the EMoptimiseStrata()
#' @param iter iter Maximum number of iterations (= generations) of the genetic algorithm (default is 200)
#' @param pops The dimension of each generations in terms of individuals (default is 50).
#' @export
#'
EMoptimiseStrata <- function(EM, objective.labels, new.strata.label = "__new__", iter = 300, pops = 50) {
  EMOrganiseData <- function(EM, fleet) {
    base <- as.vector(EM$base$table$data)
    row <- as.vector(row(EM$base$table$data))
    col <- as.vector(col(EM$base$table$data))
    fleet <- as.vector(EM$fleet[[fleet]]$table$data)
    names(fleet) <- paste("fleet", names(EM$fleet), sep = ".")
    res <- data.frame(id = 1:length(base), row, col, base, fleet)
    return(res)
  }
  if (length(objective.labels) <= 1) {
    stop("Optimisation can only occur over multiple objectives. Less than 2 objectives were specified")
  }
  encounters <- sapply(EM$objective[names(EM$objective) %in% objective.labels], function(x) x$encounter)
  fleet <- sapply(EM$encounter[names(EM$encounter) %in% encounters], function(x) x$fleet)
  if (length(unique(fleet)) != 1) {
    stop("Multiple objectives have been defined, and they must also correspond to the same fleet")
  }
  fleet <- unique(fleet)
  species <- sapply(EM$encounter[names(EM$encounter) %in% encounters], function(x) x$species)
  CVs <- sapply(EM$objective[names(EM$objective) %in% objective.labels], function(x) x$cv)
  Costs <- sapply(EM$species[names(EM$species) %in% species], function(x) x$cost)
  x <- EMOrganiseData(EM, fleet = fleet)
  y <- list()
  for (i in 1:length(objective.labels)) {
    ans <- EMsample(EM, objective.label = objective.labels[i], N = 100, quiet = TRUE, specification.only = TRUE)
    y[[i]] <- ans$parameters
  }
  mus <- data.frame(sapply(y, function(x) {
    return(x$mu)
  }))
  names(mus) <- paste0(species, ".mu")
  sds <- data.frame(sapply(y, function(x) {
    return(x$mu)
  }))
  names(sds) <- paste0(species, ".sd")
  x <- cbind(x, mus, sds)
  x$DOM <- 1

  frame <- SamplingStrata::buildFrameDF(
    df = x,
    id = "id",
    Y = paste0(species, ".mu"),
    X = "id",
    domainvalue = "DOM"
  )
  frame$WEIGHT <- ifelse(x$fleet > 0, 1, 0) # assign weights
  strata <- SamplingStrata::buildStrataDF(frame, progress = FALSE, verbose = FALSE)

  strata <- strata[order(strata$X1), ]
  for (i in 1:length(y)) {
    strata[, paste0("M", i)] <- y[[i]]$mu
    strata[, paste0("S", i)] <- y[[i]]$sd
  }
  strata$N <- ifelse(is.na(strata$N), 0, strata$N)
  cv <- data.frame("DOM" = 1, domainvalue = 1)
  for (i in 1:length(species)) {
    cv <- cbind(cv, rep(CVs[i], 1))
    names(cv)[ncol(cv)] <- paste0("CV", i)
  }
  strata2 <- strata[strata$N > 0, ]
  solution <- SamplingStrata::optimizeStrata(errors = cv, strata = strata2, iter = iter, pops = pops, parallel = TRUE, cores = NA, minnumstr = 3, showPlot = FALSE)
  stopImplicitCluster()
  strata$strata <- 0
  strata$strata[strata$N > 0] <- solution$indices
  fraction <- data.frame("strata" = solution$aggr_strata$STRATO, "fraction" = solution$aggr_strata$N)
  fraction$fraction <- fraction$fraction / Sum(fraction$fraction)
  if (length(strata$N[strata$N > 0]) > 0) {
    fraction <- rbind(c(0, 0), fraction)
  }
  # save new stratification
  new.strata <- list("table" = list("data" = matrix(strata$strata, ncol = EM$model$map_cols, byrow = FALSE)), "fraction" = fraction)
  # Update object and return
  EM2 <- EM
  EM2[["strata"]][[new.strata.label]] <- new.strata
  cat("\n")
  # return object
  return(EM2)
}
