#' Filter Project FeederWatch Data by Region
#'
#' This function filters Project FeederWatch data to
#' include only specified states, provinces, or countries.
#'
#' @param data A Project FeederWatch dataset.
#' @param regions A character vector of regions (e.g., "Washington", "United States").
#'
#' @return A filtered dataset containing only the selected regions.
#' @examplesIf interactive()
#' # Download/load example dataset
#' data <- pfw_example
#'
#' # Filter for data only from Washington using the state name
#' data_WA <- pfw_region(data, "Washington")
#'
#' # Filter for data only from Washington using the state code
#' data_WA <- pfw_region(data, "US-WA")
#'
#' # Filter for data from Washington, Oregon,
#' # and California using the state name
#' data_westcoastbestcoast <- pfw_region(data, c("Washington", "Oregon", "California"))
#'
#' @export
pfw_region <- function(data, regions) {
  # Load the region lookup table
  region_lookup <- load_region_lookup()

  # Ensure selected regions list is a vector
  if (!is.character(regions)) {
    stop("Regions must be a character vector (e.g., c('Washington', 'California'))", call. = FALSE)
  }

  # Get existing filters
  existing_filters <- attr(data, "pfw_filters")
  if (is.null(existing_filters)) {
    existing_filters <- list()
  }

  # Find matching SUBNATIONAL1_CODE values
  matching_codes <- unique(c(
    region_lookup$Code[region_lookup$Area %in% regions],
    regions[regions %in% region_lookup$Code]
  ))

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

  # Stop if there is no match and suggest a correct one for typos
  supported_countries <- c("United States", "Canada", "Mexico")
  unmatched <- regions[!regions %in% region_lookup$Area &
                         !regions %in% region_lookup$Code &
                         !regions %in% supported_countries]

  if (length(unmatched) > 0) {
    suggestion <- character()

    for (missing_loc in unmatched) {
      distances <- stringdist::stringdist(missing_loc, region_lookup$Area, method = "lv")
      min_dist <- min(distances)

    # Find the correct region
      if (min_dist <= 4) {
        best_match <- region_lookup$Area[which.min(distances)]
        suggestion <- c(suggestion, paste0("Did you mean: ", best_match, "?"))
      }
    }

      if (length(suggestion) > 0) {
    stop("One or more regions did not match the lookup table.
Check spelling or use 'load_region_lookup()' to see available names. \n",
         paste(suggestion, collapse = "\n"), call. = FALSE)
      } else {
        stop("No matching regions found. Check spelling or use 'load_region_lookup()' to see available names.",
             call. = FALSE)
        }
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
