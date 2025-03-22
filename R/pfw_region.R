#' Filter Project FeederWatch Data by Region
#'
#' This function filters PFW data to include only specified states, provinces, or countries.
#'
#' @param data The active PFW dataset (a data.table or data.frame) in your R project.
#' @param regions A character vector of regions (e.g., "Washington", "United States").
#' @return A filtered dataset containing only the selected regions.
#' @export
pfw_region <- function(data, regions) {
  # Load the region lookup table
  region_lookup <- load_region_lookup()

  # Ensure selected regions list is a vector
  if (!is.character(regions)) {
    stop("Regions must be a character vector (e.g., c('Washington', 'California'))")
  }

  # Get existing filters
  existing_filters <- attr(data, "pfw_filters")
  if (is.null(existing_filters)) {
    existing_filters <- list()
  }

  # Find matching SUBNATIONAL1_CODE values
  matching_codes <- region_lookup$Code[region_lookup$Area %in% regions]

  # Handle country-level filtering
  if ("United States" %in% regions) {
    matching_codes <- c(matching_codes, region_lookup$Code[grepl("^US-", region_lookup$Code)])
  }
  if ("Canada" %in% regions) {
    matching_codes <- c(matching_codes, region_lookup$Code[grepl("^CA-", region_lookup$Code)])
  }
  if ("Mexico" %in% regions) {
    matching_codes <- c(matching_codes, region_lookup$Code[grepl("^MX-", region_lookup$Code)])
  }

  if (length(matching_codes) == 0) {
    stop("No matching regions found. Check spelling or use 'load_region_lookup()' to see available names.")
  }

  # Filter the data
  region_filtered_data <- data[data$SUBNATIONAL1_CODE %in% matching_codes, ]

  # Remove previous region filters
  existing_filters <- Filter(function(f) f$type != "region", existing_filters)

  # Add new region filter
  region_filter <- list(type = "region", value = regions)
  updated_filters <- append(existing_filters, list(region_filter))

  # Attach updated filters
  attr(region_filtered_data, "pfw_filters") <- updated_filters

  message(length(unique(region_filtered_data$SUBNATIONAL1_CODE)), " regions successfully filtered.")
  return(region_filtered_data)
}
