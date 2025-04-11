#' Do Taxonomic Rollup on Project FeederWatch Data
#'
#' This function removes spuhs, hybrids, and slashes and
#' "demotes" subspecies/subspecies intergrades to their parent species.
#'
#' @param data A Project FeederWatch dataset.
#'
#' @return A cleaned dataset with only species-level codes and a rollup attribute.
#' @export
pfw_rollup <- function(data) {
  if (missing(data) || is.null(data)) {
    stop("No dataset provided.") # nocov
  }

  # Get existing filters before modifying data
  existing_filters <- attr(data, "pfw_filters")
  if (is.null(existing_filters)) {
    existing_filters <- list()
  }

  # Load the species translation table
  taxonomy_path <- list.files(
    path = system.file("extdata/SpeciesTranslationTable", package = "PFW"),
    pattern = "\\.csv$",
    full.names = TRUE
  )

  if (length(taxonomy_path) == 0) {
    stop("No species translation table found. Run `update_taxonomy()` to download the latest version.") # nocov
  }

  # Read in translation table
  species_table <- read.csv(taxonomy_path[1], stringsAsFactors = FALSE)

  # Create a lookup for demoting subspecies/intergrades
  subspecies_lookup <- setNames(species_table$alt_full_spp_code, species_table$species_code)

  # Filter out NA (spuhs shouldn't be included in recode())
  subspecies_lookup <- subspecies_lookup[!is.na(subspecies_lookup)]

  # Identify spuh and hybrid/slash codes for removal
  spuh_codes <- species_table$species_code[grepl("sp\\.", species_table$scientific_name, ignore.case = TRUE)]
  hybrid_codes <- species_table$species_code[grepl("^.0", species_table$species_code)]

  # Apply rollup
  cleaned_data <- data |>
    dplyr::mutate(
      SPECIES_CODE = dplyr::recode(SPECIES_CODE, !!!subspecies_lookup, .default = SPECIES_CODE)
    ) |>
    dplyr::filter(
      !SPECIES_CODE %in% spuh_codes,
      !SPECIES_CODE %in% hybrid_codes
    )

  # Remove previous rollup filters
  existing_filters <- Filter(function(f) f$type != "rollup", existing_filters)

  # Add rollup flag
  rollup_filter <- list(type = "rollup", value = TRUE)
  updated_filters <- append(existing_filters, list(rollup_filter))
  attr(cleaned_data, "pfw_filters") <- updated_filters

  message("Species roll-up complete. ", nrow(data) - nrow(cleaned_data), " ambiguous records removed.")
  return(invisible(cleaned_data))
}
