#' Update the Project FeederWatch Species Translation Table
#'
#' This function downloads the latest species translation
#' table from the Project FeederWatch website and saves it to
#' a local directory. If a previous version exists in the local
#' directory, the user will be asked for confirmation before
#' overwriting it. This ensures taxonomy can readily be kept
#' up to date annually, since it will only be manually updated
#' on the PFW website otherwise.
#'
#' @param user_dir Optional. A custom directory to write the translation table to.
#' Using the default local directory is highly recommended.
#'
#' @return A message confirming whether the update was successful.
#' @examplesIf interactive()
#' # Prompt a species translation table taxonomy update
#' update_taxonomy()
#'
#' @export
update_taxonomy <- function(user_dir = tools::R_user_dir("PFW", "data")) { # nocov start

  # Ensure the SpeciesTranslationTable directory exists
  translation_folder <- Sys.getenv("PFW_TRANSLATION_DIR", unset = file.path("inst", "extdata", "SpeciesTranslationTable"))
  if (!dir.exists(translation_folder)) {
    check_taxonomy()
  }

  # CRAN-friendly writable location for the user-updated translation table
  active_translation_table_dir <- file.path(user_dir, "SpeciesTranslationTable")

  # Check if the local directory already exists, create it if not
  if (!dir.exists(active_translation_table_dir)) {
    dir.create(active_translation_table_dir, recursive = TRUE)
    message("Created a local directory for the updated translation table at: ", active_translation_table_dir)
  }

  # Check for internet connection
  if (!curl::has_internet()) {
    stop("Unable to update taxonomy; no internet connection found. Please reconnect to the internet
         and try again.", call. = FALSE)
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
      "to download this file manually.", call. = FALSE)
  } # nocov end

  # Construct the full URL of the species translation table, since it updates every year
  full_url <- hrefs[[1]]
  if (!grepl("^http", full_url)) {
    full_url <- paste0("https://feederwatch.org", full_url)
  }

  # Define the save location
  taxonomy_path <- file.path(active_translation_table_dir, "PFW_spp_translation_table.csv")
  timestamp_path <- file.path(active_translation_table_dir, ".last_modified")

  # nocov start

  # HEAD request to get Last-Modified date
  head_resp <- httr2::request(full_url) |>
    httr2::req_user_agent("PFW R package") |>
    httr2::req_method("HEAD") |>
    httr2::req_perform()

  last_modified <- httr2::resp_header(head_resp, "last-modified")

  # If any .csv file exists in the folder, ask before overwriting
  existing_files <- list.files(active_translation_table_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(existing_files) > 0) {
    message("A species translation table file already exists.")

    local_modified <- if (file.exists(timestamp_path)) {
      readLines(timestamp_path, warn = FALSE)
    } else {
      NA_character_
    }

    if (identical(last_modified, local_modified)) {
      response <- Sys.getenv("PFW_TEST_RESPONSE", unset = "ask")
      if (response == "ask") {
        response <- readline(prompt = "The downloaded species translation table is up to date. Overwrite anyway? (y/n):")
      }
      if (tolower(response) != "y") {
        message("Update canceled.")
        return(invisible(NULL))
      }
    } else {
      message("A new species translation table version is available. Updating...")
    }
  }
  # nocov end

  # Download and save the file
  # nocov start
  httr2::request(full_url) |>
    httr2::req_user_agent("PFW R package") |>
    httr2::req_perform(path = taxonomy_path)

  # Save Last-Modified timestamp
  writeLines(last_modified, timestamp_path)

  message(
    paste0("Species translation table updated successfully at ",
           active_translation_table_dir, "\n",
    "You may need to restart your R session to use the updated version.\n")
  )
} # nocov end
