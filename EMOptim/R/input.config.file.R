#' Model configuration write function
#'
#' This function reads a EM optimisation configuration file and returns a list object in R. Where each element is a command and subcommand from the EM definition file
#'
#' @author Alistair Dunn & Johanna Pierre
#' @param file The name of the input file containing EM model configuration
#' @param path The path to the file (optional)
#' @param quiet Suppress print statements to screen (default is TRUE).
#' @param NAs Specify values used to represent missing or invalid values (i.e, NA, -999, etc), Defaults to c("NA", ".", "-", "-999")
#' @param rescale If true, then rescale the species distribtuins to have maximum=1
#' @export
#'
"input.config.file" <- function(filename, path = "", quiet = FALSE, NAs = c("NA", ".", "-", "-999"), rescale = TRUE) {
  # Internal Functions
  set.class <- function(object, new.class) {
    # use in the form
    #  object <- set.class(object,"new class")
    attributes(object)$class <- c(new.class, attributes(object)$class[attributes(object)$class != new.class])
    object
  }
  make.filename <- function(file, path) {
    if (path != "") {
      plc <- substring(path, nchar(path))
      if (!(plc == "\\" | plc == "/")) path <- paste(path, "/", sep = "")
    }
    filename <- paste(path, file, sep = "")
    return(filename)
  }
  convert.to.lines <- function(filename, quiet = quiet) {
    file <- scan(filename, what = "", sep = "\n", quiet = quiet)
    return(file)
  }
  get.lines <- function(lines, from = -1, to = -1, contains = "", starts.with = "", clip.to = "", clip.from = "", clip.to.match = "", clip.from.match = "", ...) {
    result <- lines
    if (from > 0) {
      result <- result[(1:length(result)) >= from]
    }
    if (to > 0) {
      result <- result[(1:length(result)) <= to]
    }
    if (clip.to != "") {
      if (any(result == clip.to)) {
        result <- result[(pos(result, clip.to) + 1):length(result)]
      }
    }
    if (clip.from != "") {
      if (any(result == clip.from)) {
        result <- result[1:(pos(result, clip.from) - 1)]
      }
    }
    if (clip.to.match != "") {
      if (regexp.in(result, clip.to.match)) {
        result <- result[(pos.match(result, clip.to.match) + 1):length(result)]
      }
    }
    if (clip.from.match != "") {
      if (regexp.in(result, clip.from.match)) {
        result <- result[1:(pos.match(result, clip.from.match) - 1)]
      }
    }
    if (contains != "") {
      result <- result[regexpr(contains, result, ...) > 0]
    }
    if (starts.with != "") {
      result <- result[regexpr(paste("^", starts.with, sep = ""), result, ...) > 0]
    }
    return(result)
  }
  string.to.vector.of.words <- function(string) {
    temp <- unpaste(string, sep = " ")
    return(temp[temp != ""])
  }
  strip <- function(x) {
    tmp <- unlist(strsplit(x, "\t"))
    tmp <- unlist(strsplit(tmp, " "))
    return(as.vector(paste(tmp, collapse = " ")))
  }
  unpaste <- function(string, sep) {
    return(unlist(strsplit(string, split = sep)))
  }
  is.in <- function(x, y) {
    x %in% y
  }

  ## List of valid commands
  valid_commands <- casefold(c("model", "simulations", "base_map", "strata", "fleet", "species", "encounter", "objective", "output"))
  ## List of commands without a label (there can be only one of each)
  exception_blocks <- casefold(c("model", "base_map", "simulations", "output"))
  ## List of valid subcommands
  valid_subcommands <- casefold(c(
    "label", "map_rows", "map_cols", "strata_definitions", "fleet_definitions", "species_definitions",
    "n_simulations", "sampling_rate", "max_sampling_rate", "min_sampling_rate", "steps",
    "fleet", "species", "type", "power", "table", "cost", "n", "directory", "filename",
    "lambda", "mu", "cv", "shape", "rate", "p", "theta", "min_value", "encounter"
  ))
  numeric_subcommands <- casefold(c(
    "map_rows", "map_cols", "n_simulations", "sampling_rate", "max_sampling_rate", "min_sampling_rate", "steps",
    "power", "cost", "n", "lambda", "mu", "cv", "shape", "rate", "shape1", "shape2", "theta", "min_value", "p"
  ))
  ## List of valid tables
  valid_tables <- c("data")

  ## if no path specified look in current directory
  if (missing(path)) path <- ""
  filename <- make.filename(path = path, file = filename)
  ## read filename
  file <- convert.to.lines(filename, quiet = quiet)
  ## remove white space at the beginning of a subcommand or command
  while (any(regexpr(" ", file) == TRUE)) {
    index <- regexpr(" ", file) == TRUE
    file <- ifelse(index, substring(file, 2), file)
  }
  ## Find and remove any lines that begin and end with { or } (comment block)
  index1 <- ifelse(substring(file, 1, 2) == "{", 1:length(file), 0)
  index2 <- ifelse(substring(file, 1, 2) == "}", 1:length(file), 0)
  index1 <- index1[index1 != 0]
  index2 <- index2[index2 != 0]
  if (length(index1) != length(index2)) {
    stop(paste("Error in the file ", filename, ". Cannot find a matching '{' or '}'", sep = ""))
  }
  if (length(index1) > 0 || length(index2) > 0) {
    index <- unlist(apply(cbind(index1, index2), 1, function(x) seq(x[1], x[2])))
    file <- file[!is.in(1:length(file), index)]
  }
  ## Remove any lines that begin with a comment char (#)
  file <- ifelse(regexpr("#", file) > 0, substring(file, 1, regexpr("#", file) - 1), file)
  ## Remove blank lines
  file <- file[file != ""]
  ## Check first line starts with @
  if (substring(file[1], 1, 1) != "@") {
    stop(paste("Error reading the file ", filename, ". Cannot find a '@' at the beginning of the file", sep = ""))
  }
  ## Convert tabs to spaces
  file <- as.vector(tapply(file, 1:length(file), strip))
  blocks <- get.lines(file, starts.with = "\\@", fixed = F)
  ## tables that are just matrices
  ans <- list()
  print(paste("The parameter file has", length(file[substring(file, 1, 1) == "@"]), "commands, and", length(file), "lines"), quote = FALSE)
  CommandCount <- 0
  ## A global variable if we are reading a table
  in_table <- FALSE
  ## Dummy label
  default_label <- 1
  ## Dummy variables
  Header <- 1
  Period <- NA
  ## read the file
  for (i in 1:length(file)) {
    temp <- string.to.vector.of.words(file[i])
    ## Check if it is the beginning of a command block
    if (substring(temp[1], 1, 1) == "@") {
      if (in_table) {
        stop(paste0("an @Command was found inside a table (", paste(temp, collapse = " "), ")"))
      }
      ## create a block
      Header <- 1
      CommandCount <- CommandCount + 1
      Command <- casefold(substring(temp[1], 2))
      if (!is.in(Command, valid_commands)) {
        stop(paste("@Command '", Command, "' is not a valid command name", sep = ""))
      }
      if (!is.in(Command, exception_blocks) && (length(temp) != 2)) {
        stop(paste("@Command '", Command, "' does not have a label", sep = ""))
      }
      if (is.in(Command, exception_blocks) && (length(temp) != 1)) {
        stop(paste("@Command '", Command, "' should not have a label", sep = ""))
      }
      if (is.in(Command, exception_blocks)) {
        this_label <- ""
        if (!quiet) print(paste("Reading command: '@", Command, "'", sep = ""), quote = FALSE)
      } else {
        this_label <- temp[2]
        if (!quiet) print(paste("Reading command: '@", Command, "' with label '", this_label, "'", sep = ""), quote = FALSE)
        if (exists(Command, where = ans)) {
          if (exists(this_label, where = ans[[Command]])) {
            stop(paste0("The label (", this_label, ") is duplicated for the command '", Command, "'"))
          }
        }
      }
      next
    }
    ## only two types of subcommands: tables and vectors
    if (in_table || any(get.table.subcommands()$command == casefold(temp[1]))) {
      type <- "table"
    } else {
      type <- "vector"
    }
    ## Check if it is a valid type. then read
    if (type == "vector") {
      if (!quiet) print(paste("  Reading vector parameter: '", paste(temp, collapse = " "), "'", sep = ""), quote = FALSE)
      Subcommand <- casefold(temp[1])
      if (!is.in(Subcommand, valid_subcommands)) {
        stop(paste("the subcommand ", Subcommand, " in @Command '", Command, "' is not a valid subcommand name", sep = ""))
      }
      Values <- temp[-1]
      if (is.in(Subcommand, numeric_subcommands)) {
        Values[Values %in% NAs] <- " "
        Values <- as.numeric(Values)
      }
      if (this_label == "") {
        ans[[Command]][[Subcommand]] <- Values
      } else {
        ans[[Command]][[this_label]][[Subcommand]] <- Values
      }
    } else if ((type == "table") || in_table) {
      if (!quiet) print(paste("  Reading table line: '", paste(temp, collapse = " "), "'", sep = ""), quote = FALSE)
      ## we are in a table
      in_table <- TRUE
      ## we've reached the end of the table
      if (temp[1] == "end_table") {
        in_table <- FALSE
        Header <- 1
        dimnames(mat) <- NULL
        if (!is.in(casefold(Command), c("strata"))) {
          mat[mat %in% NAs] <- " "
          mat <- matrix(as.numeric(mat), ncol = ncol(mat))
        }
        eval(parse(text = paste("ans[['", Command, "']][['", this_label, "']]$table$", Label, " = mat", sep = "")))
        Label <- ""
        Period <- NA
        next
      }
      ## we are still in a table
      if (Header == 1) {
        Label <- temp[2]
        mat <- matrix()
      } else if (Header == 2) {
        mat <- temp
      } else if (Header > 2) {
        mat <- rbind(mat, temp)
      }
      Header <- Header + 1
    } else {
      stop(paste("Invalid command/subcommand '", temp[1], "'"))
    }
  }
  ########################################################################################
  ## Sanity checks
  ########################################################################################
  # Recode base
  ans$base$table$data <- ans$base_map[[1]]$table$data
  ans$base_map <- NULL
  # Check base has valid values
  if (any(is.na(ans$base$table$data))) {
    stop("NAs found in base map. Use either 1 to indicate a valid cell, or 0 to exclude")
  }
  if (any(!ans$base$table$data %in% c(0, 1))) {
    stop("Base_map values must be either 0 or 1")
  }
  # Check objectives have correct parameters
  for (i in 1:length(ans$objective)) {
    test <- ans$objective[[i]]
    if (!"cv" %in% names(test)) {
      stop("Objective with label ", names(ans$objective)[i], " is missing the target cv parameter")
    }
  }
  # Check dimensions of all tables
  DIM <- c(ans$model$map_rows, ans$model$map_cols)
  if (any(DIM != dim(ans$base$table$data))) {
    stop("The base map does not have dimensions c(", paste(DIM, collapse = ", "), ")")
  }
  for (i in 1:length(ans$fleet)) {
    if (any(DIM != dim(ans$fleet[[i]]$table$data))) {
      stop("The fleet map with label ", names(ans$fleet)[i], " does not have dimensions c(", paste(DIM, collapse = ", "), ")
      Supplied dimensions of the fleet map were c(", paste0(dim(ans$fleet[[i]]$table$data), collapse = ", "), ").")
    }
  }
  for (i in 1:length(ans$species)) {
    if (any(DIM != dim(ans$species[[i]]$table$data))) {
      stop("The species map with label ", names(ans$species)[i], " does not have dimensions c(", paste(DIM, collapse = ", "), ")
      Supplied dimensions of the species map were c(", paste0(dim(ans$species[[i]]$table$data), collapse = ", "), ").")
    }
  }
  for (i in 1:length(ans$strata)) {
    if (any(DIM != dim(ans$strata[[i]]$table$data))) {
      stop("The strata map with label ", names(ans$strata)[i], " does not have dimensions c(", paste(DIM, collapse = ", "), ")
      Supplied dimensions of the strata map were c(", paste0(dim(ans$strata[[i]]$table$data), collapse = ", "), ").")
    }
  }
  # Check definitions of fleets
  if (any(sort(ans$model$fleet_definitions) != sort(names(ans$fleet)))) {
    stop("The fleet_definitions do not match the fleets supplied.
    Valid fleet definitions were c(", paste(ans$model$fleet_definitions, collapse = ", "), ").
    Supplied fleets were c(", paste(names(ans$fleet), collapse = ", "), ").")
  }
  if (any(sort(ans$model$strata_definitions) != sort(names(ans$strata)))) {
    stop("The strata_definitions do not match the strata supplied.
    Valid strata definitions were c(", paste(ans$model$strata_definitions, collapse = ", "), ").
    Supplied strata were c(", paste(names(ans$strata), collapse = ", "), ").")
  }
  if (any(sort(ans$model$species_definitions) != sort(names(ans$species)))) {
    stop("The species_definitions do not match the species supplied.
    Valid species definitions were c(", paste(ans$model$species_definitions, collapse = ", "), ").
    Supplied species were c(", paste(names(ans$species), collapse = ", "), ").")
  }
  # fix species tables to have max. one (i.e., the relative distribution to a maximum value of 100%)
  if (rescale) {
    for (i in 1:length(ans$species)) {
      max.value <- Max(ans$species[[i]]$table$data)
      if (max.value != 1) {
        warning("Rescaling species with label = ", names(ans$species)[i], " to have maximum of one")
        ans$species[[i]]$table$data <- ans$species[[i]]$table$data / max.value
      }
    }
  }
  # Remove species and fleet values outside of valid base map
  for (i in 1:length(ans$fleet)) {
    ans$fleet[[i]]$table$data <- ans$fleet[[i]]$table$data * ans$base$table$data
  }
  for (i in 1:length(ans$species)) {
    ans$species[[i]]$table$data <- ans$species[[i]]$table$data * ans$base$table$data
  }
  # fix fleet/species tables so that value = NA when base = 0,
  #     and then if base = 0 and value is.na(), then set to zero
  for (i in 1:length(ans$fleet)) {
    ans$fleet[[i]]$table$data[ans$base$table$data != 0 & is.na(ans$fleet[[i]]$table$data)] <- 0
    ans$fleet[[i]]$table$data[ans$base$table$data == 0] <- NA
  }
  for (i in 1:length(ans$species)) {
    ans$species[[i]]$table$data[ans$base$table$data != 0 & is.na(ans$species[[i]]$table$data)] <- 0
    ans$species[[i]]$table$data[ans$base$table$data == 0] <- NA
  }
  # Check encounter definitions
  for (i in 1:length(ans$encounter)) {
    ENC <- names(ans$encounter)[i]
    if (casefold(ans$encounter[[i]]$type) %in% "binomial") {
      if (!exists("p", ans$encounter[[i]])) {
        stop("Parameter 'p' must be specified for the binomial in the encounter definition with label '", ENC, "'")
      }
    } else if (casefold(ans$encounter[[i]]$type) %in% "lognormal") {
      if (!exists("mu", ans$encounter[[i]])) {
        stop("Parameter 'mu' must be specified for the logormal in the encounter definition with label '", ENC, "'")
      }
      if (!exists("cv", ans$encounter[[i]])) {
        stop("Parameter 'cv' must be specified for the lognormal in the encounter definition with label '", ENC, "'")
      }
    } else if (casefold(ans$encounter[[i]]$type) %in% "negative_binomial") {
      if (!exists("mu", ans$encounter[[i]])) {
        stop("Parameter 'mu' must be specified for the negative_binomial in the encounter definition with label '", ENC, "'")
      }
      if (!exists("theta", ans$encounter[[i]])) {
        stop("Parameter 'theta' must be specified for the negative_binomial in the encounter definition with label '", ENC, "'")
      }
    } else if (casefold(ans$encounter[[i]]$type) %in% "normal") {
      if (!exists("mu", ans$encounter[[i]])) {
        stop("Parameter 'mu' must be specified for the normal in the encounter definition with label '", ENC, "'")
      }
      if (!exists("sd", ans$encounter[[i]])) {
        stop("Parameter 'sd' must be specified for the normal in the encounter definition with label '", ENC, "'")
      }
    } else if (casefold(ans$encounter[[i]]$type) %in% "poisson") {
      if (!exists("lambda", ans$encounter[[i]])) {
        stop("Parameter 'lambda' must be specified for the Poisson in the encounter definition with label '", ENC, "'")
      }
    } else if (casefold(ans$encounter[[i]]$type) %in% "zifpoisson") {
      if (!exists("lambda", ans$encounter[[i]])) {
        stop("Parameter 'lambda' must be specified for the Poisson in the encounter definition with label '", ENC, "'")
      }
      if (!exists("p", ans$encounter[[i]])) {
        stop("Parameter 'p' must be specified for the Poisson in the encounter definition with label '", ENC, "'")
      }
    } else if (casefold(ans$encounter[[i]]$type) %in% "uniform") {
      if (!exists("min", ans$encounter[[i]])) {
        stop("Parameter 'min' must be specified for the Uniform in the encounter definition with label '", ENC, "'")
      }
      if (!exists("max", ans$encounter[[i]])) {
        stop("Parameter 'max' must be specified for the Uniform in the encounter definition with label '", ENC, "'")
      }
    } else {
      stop("The specified distribution is not known")
    }
  }
  # return object
  ans <- set.class(ans, "EM")
  ans
}
