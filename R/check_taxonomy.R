#' Check if a species translation table exists
#'
#' This function checks if at least one .csv file is present in
#' `inst/extdata/SpeciesTranslationTable/`. This is required for
#'  use of the pfw_species and pfw_filter functions.
#' If no file exists, it prompts the user to download it using
#' `update_taxonomy()`, or place a manually downloaded file there.
#'
#' @keywords internal
#' @return Logical (`TRUE` if a file exists, `FALSE` if not, with a message).
#' @export
check_taxonomy <- function() {
  # Determine the directory to check, allowing override for testing
  translation_folder <- Sys.getenv("PFW_TRANSLATION_DIR", unset = file.path("inst", "extdata", "SpeciesTranslationTable"))

  # Ensure the folder exists
  if (!dir.exists(translation_folder)) {
    message(
      "No species translation table detected. \n",
      "You can run `update_taxonomy()` to download one, or manually download it from:\n",
      "https://feederwatch.org/explore/raw-dataset-requests/ \n",
      "If downloading manually, please place the file in:\n",
      file.path(getwd(), translation_folder)
    )
    return(FALSE)
  }

  # Check for any CSV file inside the folder
  csv_files <- list.files(translation_folder, pattern = "\\.csv$", full.names = TRUE)

  # If no CSV files are found, return a warning
  if (length(csv_files) == 0) {
    message(
      "No species translation table detected. \n",
      "You can run `update_taxonomy()` to download one, or manually download it from:\n",
      "https://feederwatch.org/explore/raw-dataset-requests/ \n",
      "If downloading manually, please place the file in:\n",
      file.path(getwd(), translation_folder)
    )
    return(FALSE)
  }

  return(TRUE) # A CSV file exists, so proceed
}

# Mason here. Thanks for looking at my code! I hope you are enjoying this R package.
