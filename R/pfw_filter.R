#' Apply Multiple Filters to Project FeederWatch Data
#'
#' This function filters Project FeederWatch data by
#' species, region, and data validity.
#'
#' @param data A Project FeederWatch dataset.
#' @param species (Optional) A character vector of species names (common or scientific).
#' @param region (Optional) A character vector of region names (e.g., "Washington", "British Columbia").
#' @param year (Optional) Integer or vector of years (e.g., 2010 or 2010:2015).
#' @param month (Optional) Integer or vector of months (1–12). Supports wrapping (e.g., 11:2 = Nov–Feb).
#' @param valid (Optional, default = TRUE) Filter out invalid data. Removes rows where VALID == 0.
#' @param reviewed (Optional) If specified, filters by review status (TRUE for reviewed, FALSE for unreviewed).
#' @param rollup (Optional, default = TRUE) Automatically roll up subspecies to species level and remove spuhs, slashes, and hybrids.
#'
#' @return A filtered dataset.
#' @examples
#' \dontrun{
#' # Filter for Dark-eyed Junco, Song Sparrow, and Spotted Towhee in Washington in 2023
#' data_masonsyard <- pfw_filter(
#'   data,
#'   species = c("daejun", "sonspa", "spotow"),
#'   region = "US-WA",
#'   year = 2023
#' )
#'
#' # Filter for all data from Washington, Oregon, or California from November through February for 2001 through 2023
#' data_westcoastwinter <- pfw_filter(
#'   data,
#'   region = c("Washington", "Oregon", "California"),
#'   year = 2001:2023,
#'   month = 11:2
#' )
#'
#' # Filter for Greater Roadrunner in California, keeping only reviewed records and disabling taxonomic rollup
#' data_GRRO_CA <- pfw_filter(
#'   data,
#'   species = "Greater Roadrunner",
#'   region = "California",
#'   reviewed = TRUE,
#'   rollup = FALSE
#' )
#' }
#'
#' @export
pfw_filter <- function(data, species = NULL, region = NULL, year = NULL, month = NULL,
                       valid = TRUE, reviewed = NULL, rollup = TRUE) {
  if (missing(data) || is.null(data)) {
    stop("No dataset provided. Ensure you pass PFW data from `pfw_import()`.")
  }

  # Start with a fresh filter log
  applied_filters <- list()

  # Reinforce month range wrapping for date
  if (!is.null(month) && is.numeric(month) && length(month) > 1) {
    if (month[1] > month[length(month)]) {
      # If it's a wrapped range like 11:2
      month <- c(month[1]:12, 1:month[length(month)])
    }
  }

  # Region
  if (!is.null(region)) {
    data <- pfw_region(data, region)
    applied_filters <- append(applied_filters, list(list(type = "region", value = region)))
  }

  # Date
  if (!is.null(year) || !is.null(month)) {
    data <- pfw_date(data, year = year, month = month)
    applied_filters <- append(applied_filters, list(list(type = "date", value = list(year = year, month = month))))
  }

  # Rollup
  if (rollup) {
    data <- pfw_rollup(data)
    applied_filters <- append(applied_filters, list(list(type = "rollup", value = TRUE)))
  }

  # Valid
  if (!is.null(valid) && valid) {
    data <- data[data$VALID != 0, ]
    applied_filters <- append(applied_filters, list(list(type = "valid", value = TRUE)))
  }

  # Reviewed
  if (!is.null(reviewed)) {
    data <- data[data$REVIEWED == as.integer(reviewed), ]
    applied_filters <- append(applied_filters, list(list(type = "reviewed", value = reviewed)))
  }

  # Species
  if (!is.null(species)) {
    data <- pfw_species(data, species, suppress_ambiguous = TRUE) # Suppression here to avoid warning for slashes, which would already be removed from rollup
    applied_filters <- append(applied_filters, list(list(type = "species", value = tolower(species))))
  }

  # Set final attributes
  attr(data, "pfw_filters") <- applied_filters

  message("Filtering complete. ", nrow(data), " records remaining.")
  return(invisible(data))
}
