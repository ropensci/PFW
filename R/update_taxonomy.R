#' Update the Project FeederWatch Species Translation Table
#'
#' This function downloads the latest species translation
#' table from the Project FeederWatch website and saves it to
#' `inst/extdata/SpeciesTranslationTable/`.
#' If a previous version exists, the user will be asked for
#' confirmation before overwriting it. This ensures taxonomy can
#' readily be kept up to date annually, since it will only be
#' manually updated on the PFW website otherwise.
#'
#' @return A message confirming whether the update was successful.
#' @examplesIf interactive()
#' # Prompt a species translation table taxonomy update
#' update_taxonomy()
#'
#' @export
update_taxonomy <- function() { # nocov start

  # Ensure the SpeciesTranslationTable directory exists
  translation_folder <- Sys.getenv("PFW_TRANSLATION_DIR", unset = file.path("inst", "extdata", "SpeciesTranslationTable"))
  if (!dir.exists(translation_folder)) {
    dir.create(translation_folder, recursive = TRUE) # Create directories if missing
    message("Created 'inst/extdata/SpeciesTranslationTable/' directory.")
  }

  # Check for internet connection
  if (!curl::has_internet()) {
    stop("Unable to update taxonomy; no internet connection found. Please reconnect to the internet and try again.")
  } # nocov end

  # Read the PFW raw dataset request page
  # nocov start
  page <- httr2::request("https://feederwatch.org/explore/raw-dataset-requests/") |>
    httr2::req_user_agent("PFW R package") |>
    httr2::req_perform() |>
    httr2::resp_body_html()

  # Extract all links from the page
  links <- xml2::xml_find_all(page, ".//a[contains(@href, 'PFW_spp_translation_table_')]")
  hrefs <- xml2::xml_attr(links, "href")

  # If no link is found, stop with an error message
  if (length(hrefs) == 0) {
    stop(
      "No species translation table found. FeederWatch may have changed their webpage format.\n",
      "You can go to https://feederwatch.org/explore/raw-dataset-requests/ ",
      "to download this file manually."
    )
  } # nocov end

  # Construct the full URL of the species translation table, since it updates every year
  full_url <- hrefs[[1]]
  if (!grepl("^http", full_url)) {
    full_url <- paste0("https://feederwatch.org", full_url)
  }

  # Define the save location
  taxonomy_path <- file.path(translation_folder, "PFW_spp_translation_table.csv")

  # If any .csv file exists in the folder, ask before overwriting
  existing_files <- list.files(translation_folder, pattern = "\\.csv$", full.names = TRUE)
  if (length(existing_files) > 0) {
    message("A species translation table file already exists.")
    response <- Sys.getenv("PFW_TEST_RESPONSE", unset = "ask")

    if (response == "ask") {
      response <- readline(prompt = "Overwrite existing files? (y/n): ")
    }
  # nocov start
    if (tolower(response) != "y") {
      message("Update canceled.")
      return(invisible(NULL))
    }
  }
  # nocov end

  # Download and save the file
  # nocov start
  httr2::request(full_url) |>
    httr2::req_user_agent("PFW R package") |>
    httr2::req_perform(path = taxonomy_path)

  message(
    "Species translation table updated successfully!\n",
    "You may need to restart your R session to use the updated version.\n"
  )
} # nocov end
