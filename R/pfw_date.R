#' Filter Project FeederWatch Data by Month and/or Year
#'
#' Filters Project FeederWatch data by year and/or month, allowing range-based filtering and wrapping months around new years.
#'
#' @param data A Project FeederWatch dataset.
#' @param year Optional. Integer or vector of years (e.g., 2010 or 2010:2015).
#' @param month Optional. Integer or vector of months (1–12). Supports wrapping (e.g., c(11, 2) = Nov–Feb).
#'
#' @return A filtered dataset with date filter attributes.
#' @examples
#' \dontrun{
#' # Filter by a single year
#' data_2001 <- pfw_date(data, year = 2001)
#'
#' # Filter by multiple years
#' data_0123 <- pfw_date(data, year = 2001:2023)
#'
#' # Filter by a single month
#' data_feb <- pfw_date(data, month = 2)
#'
#' # Filter by a span of months
#' data_winter <- pfw_date(data, month = 11:2)
#'
#' # Filter by both year and month
#' data_filtered <- pfw_date(data, year = 2001:2023, month = 11:2)
#'}
#'
#' @export
pfw_date <- function(data, year = NULL, month = NULL) {
  if (missing(data) || is.null(data)) {
    stop("No dataset provided.")
  }

  # Create date column (does not overwrite original columns)
  if (!"PFW_DATE" %in% names(data) && nrow(data) > 0) {
    data$PFW_DATE <- lubridate::make_date(year = data$Year, month = data$Month, day = data$Day)
  }

  # Get existing filters
  existing_filters <- attr(data, "pfw_filters")
  if (is.null(existing_filters)) {
    existing_filters <- list()
  }

  # Filter the dataset (using a new object to preserve original attributes)
  filtered_data <- data

  # Apply year filter
  if (!is.null(year)) {
    filtered_data <- filtered_data[filtered_data$Year %in% year, ]
  }

  # Apply month filter, including wrapped ranges like 11:2
  original_month <- month # Save for attributes before modifying

  if (!is.null(month)) {
    month <- as.integer(month)
    month <- month[month %in% 1:12] # Sanitize

    # If descending and wraps, expand to wrapped form
    if (length(month) > 1 &&
      month[1] > month[length(month)] &&
      all(diff(month) == -1)) {
      month <- c(month[1]:12, 1:month[length(month)])
    }

    filtered_data <- filtered_data[filtered_data$Month %in% month, ]
  }

  # Remove any previous date filters
  existing_filters <- Filter(function(f) f$type != "date", existing_filters)

  # Add the new date filter
  date_filter <- list(type = "date", value = list(year = year, month = month))
  updated_filters <- append(existing_filters, list(date_filter))

  # Attach updated filters
  attr(filtered_data, "pfw_filters") <- updated_filters

  message("Date filtering complete. ", nrow(filtered_data), " records remaining.")
  return(invisible(filtered_data))
}
