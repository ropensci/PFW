#' Import Project FeederWatch Data
#'
#' This function reads all .csv files downloaded from the Project FeederWatch website, either from the default "data-raw/" folder
#' or from a user-specified folder. Optionally, it can apply filters like region, species, year, etc.
#'
#' @param folder The folder where Project FeederWatch data is stored. Default is "data-raw/".
#' @param filter Logical. If TRUE, applies filters using pfw_filter(). Default is FALSE.
#' @param ... Additional arguments passed to pfw_filter() for filtering (e.g., region, species, year).
#'
#' @return A combined and optionally filtered data.table containing all Project FeederWatch data.
#' @examples
#' \dontrun{
#' # Import all downloaded data from the default folder ("data-raw")
#' data <- pfw_import()
#'
#' # Import and filter for Washington checklists from 2023
#' data_filtered <- pfw_import(filter = TRUE, region = "Washington", year = 2023)
#' }
#'
#' @export
pfw_import <- function(folder = "data-raw/", filter = FALSE, ...) {
  # Ensure the folder exists
  if (!dir.exists(folder)) {
    stop("The folder '", folder, "' does not exist. Please create it and download ONLY Project FeederWatch CSV files into it.")
  }

  # List all CSV files in the folder
  files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No CSV files found in '", folder, "'. Please download ONLY Project FeederWatch data into this folder or specify the correct path.")
  }

  # Load valid PFW files one at a time
  data_list <- list()
  expected_cols <- c("SPECIES_CODE", "HOW_MANY", "SUB_ID")

  for (file in files) {
    message("Reading: ", file)
    data <- data.table::fread(file)

    # Skip if it's not a valid PFW observation file
    if (!all(expected_cols %in% names(data))) {
      message("Skipping file (incorrect data structure): ", file)
      next
    }

    data_list <- append(data_list, list(data))
    rm(data)
    gc()
  }

  if (length(data_list) == 0) {
    stop("No valid Project FeederWatch data files were found in the folder.")
  }

  # Combine all loaded data
  combined_data <- data.table::rbindlist(data_list, fill = TRUE)

  # Remove NAs in species counts
  combined_data <- dplyr::filter(combined_data, !is.na(HOW_MANY))

  # Save import path as an attribute, even if it's the default, so it can be called after a restart
  attr(combined_data, "pfw_import_path") <- folder

  # Save full data for zerofilling
  .pfw_env$full_data <- combined_data

  message(length(data_list), " PFW files successfully imported.")

  # Extract filter args
  filter_args <- list(...)

  # Trigger filtering if requested or relevant args are passed
  do_filter <- isTRUE(filter) || any(names(filter_args) %in% c("species", "region", "year", "month", "valid", "reviewed", "rollup"))

  if (do_filter) {
    if (!"valid" %in% names(filter_args)) filter_args$valid <- NULL
    if (!"rollup" %in% names(filter_args)) filter_args$rollup <- FALSE
    combined_data <- do.call(pfw_filter, c(list(combined_data), filter_args))
  }

  return(invisible(combined_data))
}
