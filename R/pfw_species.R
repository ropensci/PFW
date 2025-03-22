#' Filter Project FeederWatch Data by Species
#'
#' This function filters PFW data to include only selected species, with common names
#' or scientific names via the species translation table.
#'
#' @param data The PFW dataset (a data.table or data.frame).
#' @param species A character vector of species names (common or scientific).
#' @param suppress_ambiguous (Optional, default = FALSE) TRUE/FALSE on including missing subspecies in the warning. This is mainly as a fix for the pfw_filter function.
#' @return A filtered dataset containing only the selected species.
#' @export
pfw_species <- function(data, species, suppress_ambiguous = FALSE) {
  if (!is.character(species)) {
    stop("Species must be a character vector (e.g., c('Greater Roadrunner', 'Cactus Wren'))")
  }

  taxonomy_path <- list.files(
    path = system.file("extdata/SpeciesTranslationTable", package = "PFW"),
    pattern = "\\.csv$",
    full.names = TRUE
  )

  if (length(taxonomy_path) == 0) {
    stop("No species translation table found. Run `update_taxonomy()` to download the latest version.")
  }

  species_table <- read.csv(taxonomy_path[1], stringsAsFactors = FALSE)

  if (!all(c("species_code", "american_english_name", "scientific_name") %in% colnames(species_table))) {
    stop("Species translation table is missing required columns. Try updating it with `update_taxonomy()`.")
  }

  # Get existing filters before filtering
  existing_filters <- attr(data, "pfw_filters")
  if (is.null(existing_filters)) {
    existing_filters <- list()
  }

  # Convert user input to lowercase for case-insensitive matching
  species <- tolower(species)

  # Find species codes
  matching_codes <- unique(species_table$species_code[
    tolower(species_table$american_english_name) %in% species |
      tolower(species_table$scientific_name) %in% species
  ])

  # Include subspecies
  expanded_codes <- unique(species_table$species_code[
    species_table$species_code %in% matching_codes |
      Reduce(`|`, lapply(matching_codes, function(code) grepl(paste0("^", code), species_table$species_code)))
  ])

  # Warn for unmatched species
  not_found <- species[!species %in% tolower(species_table$american_english_name) &
    !species %in% tolower(species_table$scientific_name)]
  if (length(not_found) > 0) {
    warning("The following species were not found in the species table: ", paste(not_found, collapse = ", "))
  }

  # Filter for selected species
  species_filtered_data <- data[data$SPECIES_CODE %in% expanded_codes, ]

  # Identify species actually missing from the dataset
  present_species <- unique(data$SPECIES_CODE)
  missing_from_data <- expanded_codes[!expanded_codes %in% present_species]

  # Report missing species from this dataset
  if (length(missing_from_data) > 0) {
    missing_names <- unique(species_table$american_english_name[
      species_table$species_code %in% missing_from_data
    ])

    # Suppress ambiguous names if requested
    if (suppress_ambiguous) {
      missing_names <- missing_names[!grepl("\\)", missing_names)]
    }

    if (length(missing_names) > 0) {
      message(
        "Species selected in filter but absent from input data: ",
        paste(missing_names, collapse = ", ")
      )
    }
  }

  # Remove previous species filters
  existing_filters <- Filter(function(f) f$type != "species", existing_filters)

  # Add new species filter
  species_filter <- list(type = "species", value = species)
  updated_filters <- append(existing_filters, list(species_filter))

  # Attach updated filters
  attr(species_filtered_data, "pfw_filters") <- updated_filters

  message(length(unique(species_filtered_data$SPECIES_CODE)), " species successfully filtered.")
  return(species_filtered_data)
}
