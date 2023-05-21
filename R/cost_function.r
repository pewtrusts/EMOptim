#' Utility function
#'
#' @author Alistair Dunn & Johanna Pierre
#' @param type type of cost function
#' @param N Number of events to apply the cost function to
#' @param fleet the fleet cost per event for every event in the fleet
#' @param species the species cost per event sampled (N)
#' @keywords internal
#'
"cost_function" <- function(type, N_fleet, fleet, N_species, species) {
  if (type == "standard") {
    fleet.cost <- N_fleet * fleet
    species.cost <- N_species * species
    total <- fleet.cost + species.cost
  }
  return(list("cost_function" = type, "total" = total, "fleet" = fleet.cost, "species" = species.cost))
}
