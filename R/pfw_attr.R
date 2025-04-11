#' View Filter Attributes on Manipulated Project FeederWatch Data
#'
#' This function allows users to view all filters they've applied
#' to a filtered Project FeederWatch dataset by printing its
#' recorded filter attributes in a readable format.
#'
#' @param data A filtered Project FeederWatch dataset.
#'
#' @return A named list of applied filters.
#' @examples
#' \dontrun{
#' # View filters applied to your active data
#' pfw_attr(filtered_data)
#' }
#'
#' @export
pfw_attr <- function(data) {
  filters <- attr(data, "pfw_filters")

  if (is.null(filters) || length(filters) == 0) {
    message("No filters found on this dataset.")
    return(invisible(NULL))
  }

  cat("Filters applied to this dataset:\n\n")
  for (f in filters) {
    if (is.list(f$value) && !is.null(names(f$value))) {
      # Handle named list (e.g., year + month from date filter)
      cat("- Filter type:", f$type, "\n")
      for (n in names(f$value)) {
        cat("  ", n, ":", paste(f$value[[n]], collapse = ", "), "\n")
      }
      cat("\n")
    } else {
      # Handle basic filter types
      cat("- Filter type:", f$type, "\n  Values:", paste(f$value, collapse = ", "), "\n\n")
    }
  }

  invisible(filters)
}
