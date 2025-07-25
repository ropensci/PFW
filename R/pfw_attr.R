#' View Filter Attributes on Manipulated Project FeederWatch Data
#'
#' This function allows users to view all filters they've applied
#' to a filtered Project FeederWatch dataset by printing its
#' recorded filter attributes in a readable format.
#'
#' @param data A filtered Project FeederWatch dataset.
#'
#' @return A named list of applied filters.
#' @examplesIf interactive()
#' # Download/load example dataset
#' data <- pfw_example
#'
#' # Filter for Dark-eyed Junco
#' filtered_data <- pfw_species(data, "Dark-eyed Junco")
#'
#' # View filters applied to your active data
#' pfw_attr(filtered_data)
#'
#' @export
pfw_attr <- function(data) {
  filters <- attr(data, "pfw_filters")

  if (is.null(filters) || length(filters) == 0) {
    message("No filters found on this dataset.")
    return(invisible(NULL))
  }

  message("Filters applied to this dataset:\n\n")
  for (f in filters) {
    if (is.list(f$value) && !is.null(names(f$value))) {
      # Handle named list (e.g., year + month from date filter)
      message("- Filter type:", f$type, "\n")
      for (n in names(f$value)) {
        message("  ", n, ":", paste(f$value[[n]], collapse = ", "), "\n")
      }
      message("\n")
    } else {
      # Handle basic filter types
      message("- Filter type:", f$type, "\n  Values:", paste(f$value, collapse = ", "), "\n\n")
    }
  }

  invisible(filters)
}
