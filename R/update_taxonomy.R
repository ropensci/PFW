#' Update the Project FeederWatch Species Translation Table
#'
#' This function downloads the latest species translation table from the Project FeederWatch website
#' and saves it to `inst/extdata/SpeciesTranslationTable/`. If a previous version exists, the user will be asked
#' for confirmation before overwriting it. This ensures taxonomy can readily be kept up to date annually,
#' since it will only be manually updated on the PFW website otherwise.
#'
#' @return A message confirming whether the update was successful.
#' @export
update_taxonomy <- function() {

  # Ensure the SpeciesTranslationTable directory exists
  translation_folder <- Sys.getenv("PFW_TRANSLATION_DIR", unset = file.path("inst", "extdata", "SpeciesTranslationTable"))
  if (!dir.exists(translation_folder)) {
    dir.create(translation_folder, recursive = TRUE) # Create directories if missing
    message("Created 'inst/extdata/SpeciesTranslationTable/' directory.")
  }

  # Read the PFW raw dataset request page
  page <- rvest::read_html("https://feederwatch.org/explore/raw-dataset-requests/")

  # Extract all links from the page
  links <- page |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  # Find the link that contains "PFW_spp_translation_table"
  taxonomy_link <- links[grepl("PFW_spp_translation_table_", links)]

  # If no link is found, stop with an error message
  if (length(taxonomy_link) == 0) {
    stop(
      "No species translation table found. FeederWatch may have changed their webpage format.\n",
      "You can go to https://feederwatch.org/explore/raw-dataset-requests/ ",
      "to download this file manually."
    )
  }

  # Construct the full URL of the species translation table, since it updates every year
  full_url <- taxonomy_link
  if (!grepl("^http", taxonomy_link)) {
    full_url <- paste0("https://feederwatch.org", taxonomy_link)
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

    if (tolower(response) != "y") {
      message("Update canceled.")
      return()
    }
  }


  # Download and save the file
  utils::download.file(full_url, taxonomy_path, mode = "wb")

  message(
    "Species translation table updated successfully!\n",
    "You may need to restart your R session to use the updated version.\n"
  )
}
