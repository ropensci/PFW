#' Load Region Lookup Table
#'
#' Load/calls the region lookup table.
#'
#' @keywords internal
#' @return A data frame containing columns `Code` (SUBNATIONAL1_CODE) and `Area` (region name).
#' @noRd
load_region_lookup <- function() {
  region_lookup
}
