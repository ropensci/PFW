#' Do Taxonomic Rollup on Project FeederWatch Data
#'
#' This function removes spuhs, hybrids, and slashes and
#' "demotes" subspecies/subspecies intergrades to their parent species.
#'
#' @param data A Project FeederWatch dataset.
#'
#' @return A cleaned dataset with only species-level codes and a rollup attribute.
#' @examples
#' # Download/load example dataset
#' data <- pfw_example
#'
#' # Apply taxonomic rollup to an active PFW dataset
#' rolled_data <- pfw_rollup(data)
#'
#' @export
pfw_rollup <- function(data) {
  # Get existing filters before modifying data
  existing_filters <- attr(data, "pfw_filters")
  if (is.null(existing_filters)) {
    existing_filters <- list()
  }

  # Check for a user-updated translation table from update_taxonomy()
  local_path <- file.path(tools::R_user_dir("PFW", "data"), "SpeciesTranslationTable", "PFW_spp_translation_table.csv")

  # Fall back to packaged translation table if update_taxonomy() wasn't run
  pkg_path <- system.file("extdata/SpeciesTranslationTable/PFW_spp_translation_table.csv", package = "PFW")

  # Choose the available translation table
  taxonomy_path <- if (file.exists(local_path)) local_path else pkg_path

  if (!file.exists(taxonomy_path)) {
    stop("No species translation table found. Run `update_taxonomy()` to download the latest version.",
         call. = FALSE)
  }

  # Read in translation table
  species_table <- read.csv(taxonomy_path[1])

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

  message("Taxonomic rollup complete. ", nrow(data) - nrow(cleaned_data), " ambiguous records removed.")
  return(invisible(cleaned_data))
}
