#' Filter Project FeederWatch Data to "Standard" Seasonal Window
#'
#' Project FeederWatch's Data Users Guide (https://birdscanada.github.io/BirdsCanada_PFW/Start2.html)
#' Suggests that data should be truncated by date to avoid biases from years where the
#' Project FeederWatch survey season was extended.
#' This function filters data to include only observations within the typical FeederWatch season:
#' after November 8 and before April 3.
#'
#' @param data A Project FeederWatch dataset with Year, Month, and Day columns.
#'
#' @return A filtered dataset limited to Nov 8 – Apr 3 across years.
#' @export
pfw_truncate <- function(data) {
  if (!all(c("Year", "Month", "Day") %in% names(data))) {
    stop("Data must contain Year, Month, and Day columns.")
  }

  # Create a temporary date column for truncating
  data <- data |>
    dplyr::mutate(
      .PFW_DATE_TEMP = lubridate::make_date(Year, Month, Day),
      .PFW_DOY = lubridate::yday(.PFW_DATE_TEMP)
    ) |>
    dplyr::filter(.PFW_DOY <= 93 | .PFW_DOY >= 312) |>
    dplyr::select(-.PFW_DOY, -.PFW_DATE_TEMP)

  message("Data filtered to standard PFW season window.")
  return(invisible(data))
}
