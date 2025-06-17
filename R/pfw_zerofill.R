#' Zerofill Species not Detected in each Survey Instance for Analysis
#'
#' This function adds zeros for checklists where selected species were absent,
#' setting HOW_MANY = 0 for presence/absence-based analyses.
#' Note that zerofilling entire, unfiltered datasets from Project FeederWatch will take a long time!
#'
#' @param data A Project FeederWatch dataset, optionally filtered for species.
#'
#' @return A dataset with zerofilled values included for each species.
#' @examples
#' \dontrun{
#' # Zerofill an active PFW dataset
#' data_zf <- pfw_zerofill(data)
#' }
#'
#' @export
pfw_zerofill <- function(data) {
  filters <- attr(data, "pfw_filters")
  import_path <- attr(data, "pfw_import_path")

  # Get full data if available, or re-import if necessary
  full_data <- .pfw_env$full_data

  if (is.null(full_data)) { # nocov start
    if (is.null(import_path)) {
      stop("No full dataset available and no import path found. Run pfw_import() or use data
           with an import path attribute.", call. = FALSE)
    }

    message("Full dataset not found in memory. Re-importing...")

    # Reconstruct filter arguments for import (excluding species)
    import_filters <- list()
    if (!is.null(filters)) {
      for (f in filters) {
        if (f$type == "region") {
          import_filters$region <- f$value
        } else if (f$type == "date") {
          import_filters$year <- f$value$year
          import_filters$month <- f$value$month
        } else if (f$type == "valid") {
          import_filters$valid <- f$value
        } else if (f$type == "reviewed") {
          import_filters$reviewed <- f$value
        } else if (f$type == "rollup") {
          import_filters$rollup <- f$value
        }
      }
    }

    # Re-import and re-assign for future use
    full_data <- suppressMessages(
      do.call(pfw_import, c(list(folder = import_path, filter = TRUE), import_filters))
    )
  } # nocov end

  # Apply filters again to match data context
  if (!is.null(filters)) {
    for (f in filters) {
      if (f$type == "region") {
        full_data <- suppressMessages(pfw_region(full_data, f$value))
      } else if (f$type == "date") {
        full_data <- suppressMessages(pfw_date(full_data, year = f$value$year, month = f$value$month))
      } else if (f$type == "valid" && isTRUE(f$value)) {
        full_data <- full_data[full_data$VALID != 0, ]
      } else if (f$type == "reviewed") {
        full_data <- full_data[full_data$REVIEWED == as.integer(f$value), ]
      } else if (f$type == "rollup" && isTRUE(f$value)) {
        full_data <- suppressMessages(pfw_rollup(full_data))
      }
    }
  }

  # Create zerofilled rows
  all_checklists <- dplyr::distinct(full_data, SUB_ID, .keep_all = TRUE)
  species <- unique(data$SPECIES_CODE)
  zf_rows <- list()

  for (sp in species) {
    zeros <- all_checklists
    zeros$SPECIES_CODE <- sp
    zeros$HOW_MANY <- 0
    detected <- data$SUB_ID[data$SPECIES_CODE == sp]
    zeros <- zeros[!zeros$SUB_ID %in% detected, ]
    zf_rows[[sp]] <- zeros
  }

  # Combine and attach filter metadata
  zf_data <- dplyr::bind_rows(data, dplyr::bind_rows(zf_rows))

  if (!is.null(filters)) attr(zf_data, "pfw_filters") <- filters
  if (!is.null(import_path)) attr(zf_data, "pfw_import_path") <- import_path

  message("Zerofilling complete.")
  return(invisible(zf_data))
}
