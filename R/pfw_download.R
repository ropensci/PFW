#' Download Raw Project FeederWatch Data by Year
#'
#' This function downloads raw data for selected years from
#' the Project FeederWatch website. It unzips the downloaded data
#' and saves the .csv files into a local folder
#' (default: "data-raw/"), removing the zip files afterward.
#' It will download all files required to cover the user-selected years.
#'
#' @param years Integer or vector of years (e.g., 2001, 2001:2023, c(1997, 2001, 2023)).
#' @param folder The folder where Project FeederWatch data is stored. Default is "data-raw/".
#'
#' @return Invisibly returns the downloaded files.
#' @examplesIf interactive()
#' # Download data from 2001-2006 into the default folder
#' pfw_download(years = 2001:2006)
#'
#' @export
pfw_download <- function(years, folder = file.path(tools::R_user_dir("PFW", "data"), "data-raw")) {
  if (missing(years) || is.null(years)) stop("You must specify at least one year.")
  years <- as.integer(years)

  # Check for internet connection
  if (!curl::has_internet()) {
    stop("Unable to download data; no internet connection found. Please reconnect to the internet and try again.")
  }
  # Ensure folder exists or create it
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
    message("Created folder ", folder)
  }

  # Read webpage
  # nocov start
  page <- httr2::request("https://feederwatch.org/explore/raw-dataset-requests/") |>
    httr2::req_user_agent("PFW R package") |>
    httr2::req_perform() |>
    httr2::resp_body_html()

  # Filter for .zip folder links containing selected years
  links <- xml2::xml_find_all(page, ".//a[contains(@href, '.zip')]")
  hrefs <- xml2::xml_attr(links, "href")
  zip_links <- hrefs[grepl("PFW_all_\\d{4}_\\d{4}.*\\.zip$", hrefs)]

  # Parse year ranges and filter links that overlap with input years
  # Using this matching method will prevent issues as new years are added to the webpage's data
  matches <- lapply(zip_links, function(link) {
    match <- regmatches(link, regexec("PFW_all_(\\d{4})_(\\d{4})", link))[[1]]
    if (length(match) == 3) {
      range_years <- as.integer(match[2]):as.integer(match[3])
      overlap <- any(years %in% range_years)
      list(link = link, overlap = overlap)
    } else NULL
  })

  matched_links <- Filter(function(x) !is.null(x) && x$overlap, matches)
  if (length(matched_links) == 0) {
    message("No available data matched your specified years.")
    return(invisible(NULL))
  }

  # Ask before overwriting existing .csv files
  existing_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  if (length(existing_files) > 0) {
    response <- Sys.getenv("PFW_TEST_RESPONSE", unset = "ask")
    if (response == "ask") {
      response <- readline(prompt = "CSV files already exist. Overwrite? (y/n): ")
    }
    if (tolower(response) != "y") {
      message("Download canceled.")
      return(invisible(NULL))
    }
  }
  # Download, unzip, and clean up
  for (entry in matched_links) {
    link <- entry$link
    if (!grepl("^http", link)) {
      link <- paste0("https://feederwatch.org", link)
    }
    zip_name <- basename(link)
    zip_path <- file.path(folder, zip_name)

    httr2::request(link) |>
      httr2::req_user_agent("PFW R package") |>
      httr2::req_perform(path = zip_path)

    utils::unzip(zip_path, exdir = folder)
    unlink(zip_path)
    message("Downloaded and extracted: ", zip_name)
  }
  # nocov end
  message("Download complete. You can now use pfw_import to import your data into the active R session.")
  invisible(TRUE)
}
