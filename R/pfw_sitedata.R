#' Merge Site Metadata into Project FeederWatch Data
#'
#' This function joins habitat and site metadata into PFW observation data using the site description file.
#'
#' @param data A Project FeederWatch dataset (data.frame or data.table).
#' @param path File path to a the site description data file from https://feederwatch.org/explore/raw-dataset-requests/.
#' @return The original dataset with site metadata merged in.
#' @export
pfw_sitedata <- function(data, path) {
  if (missing(data) || is.null(data)) {
    stop("No observation dataset provided.")
  }

  if (missing(path) || !file.exists(path)) {
    stop("Valid path to site_data.csv must be provided.")
  }

  # Check for required columns in observation data
  if (!all(c("LOC_ID", "PROJ_PERIOD_ID") %in% names(data))) {
    stop("Observation data must include LOC_ID and PROJ_PERIOD_ID columns.")
  }

  # Load site metadata
  site_data <- read.csv(path, stringsAsFactors = FALSE)

  # Normalize column names for joining
  names(site_data) <- tolower(names(site_data))

  if (!all(c("loc_id", "proj_period_id") %in% names(site_data))) {
    stop("Site data must include 'loc_id' and 'proj_period_id' columns.")
  }

  # Perform join
  merged <- dplyr::left_join(data, site_data, by = c("LOC_ID" = "loc_id", "PROJ_PERIOD_ID" = "proj_period_id"))

  message("Site metadata successfully merged.")
  return(merged)
}
