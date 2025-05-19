#' Merge Site Metadata into Project FeederWatch Data
#'
#' This function joins habitat and site metadata into Project FeederWatch
#' observation data using the site description file.If the site metadata
#' file is not found, it will be downloaded automatically to
#' the designated path or "data-raw" if no path is selected.
#'
#' @param data A Project FeederWatch dataset.
#' @param path File path to the site description .csv from https://feederwatch.org/explore/raw-dataset-requests/. If not specified, defaults to "data-raw/site_data.csv".
#'
#' @return The original dataset with site metadata merged in.
#'
#' @examplesIf interactive()
#' # Download/loads the example dataset
#' data <- pfw_example()
#'
#' # Merge site metadata into example observation data
#' data_sites <- pfw_sitedata(data, "data-raw/site_data.csv")
#'
#' @export
pfw_sitedata <- function(data, path) {
  if (missing(path) || is.null(path)) {
    path <- file.path(tools::R_user_dir("PFW", "data"), "data-raw", "site_data.csv")
  }

  # Download site data if path doesn't exist
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

    # Scrape the FeederWatch raw dataset request page
    page <- httr2::request("https://feederwatch.org/explore/raw-dataset-requests/") |>
      httr2::req_user_agent("PFW R package") |>
      httr2::req_perform() |>
      httr2::resp_body_html()

    # Look for the site metadata file link
    links <- xml2::xml_find_all(page, ".//a[contains(@href, 'PFW_count_site_data_public_')]")
    hrefs <- xml2::xml_attr(links, "href")

    if (length(hrefs) == 0) { # nocov start
      stop(
        "No site metadata file found. FeederWatch may have changed their webpage format.\n",
        "You can go to https://feederwatch.org/explore/raw-dataset-requests/ ",
        "to download the file manually."
      )
    } # nocov end

    # Construct full URL
    site_url <- hrefs[[1]]
    if (!grepl("^http", site_url)) {
      site_url <- paste0("https://feederwatch.org", site_url)
    }
    message("Site metadata not found at provided path. Downloading from FeederWatch...")

    tryCatch(
      httr2::request(site_url) |>
        httr2::req_user_agent("PFW R package") |>
        httr2::req_perform(path = path),
      error = function(e) {
        stop("Failed to download site metadata: ", e$message)
      }
    )
  }

  # Check again to be safe
  if (!file.exists(path)) {
    stop("Site metadata could not be loaded. Please download it manually from: ", site_url) # nocov
  }

  # Check for required columns in observation data
  if (!all(c("LOC_ID", "PROJ_PERIOD_ID") %in% names(data))) {
    stop("Observation data must include LOC_ID and PROJ_PERIOD_ID columns.")
  }

  # Load site metadata
  site_data <- read.csv(path)

  # Normalize column names for joining
  names(site_data) <- tolower(names(site_data))

  # Perform join
  merged <- dplyr::left_join(data, site_data, by = c("LOC_ID" = "loc_id",
                                                     "PROJ_PERIOD_ID" = "proj_period_id"))

  message("Site metadata successfully merged.")
  return(invisible(merged))
}
