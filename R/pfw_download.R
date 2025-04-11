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
#' @examples
#' \dontrun{
#' # Download data from 2001-2023 into the default folder ("data-raw")
#' pfw_download(years = 2001:2023)
#' }
#'
#' @export
pfw_download <- function(years, folder = "data-raw/") {
  if (missing(years) || is.null(years)) stop("You must specify at least one year.")
  years <- as.integer(years)

  # Ensure folder exists or create it
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
    message("Created folder ", folder, " in working directory.")
  }

  # Read webpage
  page <- rvest::read_html("https://feederwatch.org/explore/raw-dataset-requests/")
  links <- page |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  # Filter for .zip folder links containing year ranges
  zip_links <- links[grepl("PFW_all_\\d{4}_\\d{4}.*\\.zip$", links)]

  # Parse year ranges and filter links that overlap with input years
  # Using this matching method will prevent issues as new years are added to the webpage's data
  matches <- lapply(zip_links, function(link) {
    match <- regmatches(link, regexec("PFW_all_(\\d{4})_(\\d{4})", link))[[1]]
    if (length(match) == 3) {
      range_years <- as.integer(match[2]):as.integer(match[3])
      overlap <- any(years %in% range_years)
      list(link = link, range = range_years, overlap = overlap)
    } else {
      NULL
    }
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
    zip_name <- basename(link)
    if (!grepl("^http", link)) {
      link <- paste0("https://feederwatch.org", link)
    }

    zip_path <- file.path(folder, zip_name)
    utils::download.file(link, zip_path, mode = "wb")
    utils::unzip(zip_path, exdir = folder)
    unlink(zip_path)
    message("Downloaded and extracted: ", zip_name)
  }

  message("Download complete. You can now use pfw_import to import your data into the active R session.")
  invisible(TRUE)
}
