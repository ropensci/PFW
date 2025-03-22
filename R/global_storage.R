# This is a file used for holding on to "temporary data"
# It's primary use is to save SUB IDs from import for use in zero-filling,
# But also to apply some global package assignments for various functions
#' @importFrom utils read.csv
#' @importFrom stats setNames
utils::globalVariables(c(
  "HOW_MANY",
  "SPECIES_CODE",
  "SUB_ID"
))

.pfw_env <- new.env(parent = emptyenv()) # Hidden environment for saving import data
