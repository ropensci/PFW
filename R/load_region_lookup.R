#' Load the region lookup table
#'
#' This function loads the region lookup table, which maps
#' SUBNATIONAL1_CODE values to region "common" names.
#' This function mostly exists to be part of the pfw_region function.
#'
#' @keywords internal
#' @return A data frame containing columns `Code` (SUBNATIONAL1_CODE) and `Area` (region name).
#' @export
load_region_lookup <- function() {
  lookup_path <- system.file("extdata", "Region_Lookup.csv", package = "PFW")

  if (lookup_path == "") { # nocov start
    stop("Region lookup table not found. Please reinstall/update the package to ensure 'Region_Lookup.csv' is in 'inst/extdata/'.")
  } # nocov end

  read.csv(lookup_path, stringsAsFactors = FALSE)
}
