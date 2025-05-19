#' Download and Load Example Project FeederWatch Dataset
#'
#' Provides a sample dataset for demonstration and testing purposes.
#' This dataset includes data from 2021 - May 2024 from
#' Washington, Oregon, and California.
#'
#' @return A dataset of example Project FeederWatch data.
#' @examples
#' # Install and load the example dataset
#' example_data <- pfw_example()
#'
#' @export
pfw_example <- function() {
  path <- system.file("extdata", "pfw_example.csv", package = "PFW")
  # If not found, download from the GitHub repository to a temp file
  if (path == "") {
    message("Local example dataset not found. Downloading from GitHub...")

    # Check for internet connection
    if (!curl::has_internet()) {
      stop("Unable to download example dataset; no internet connection found. Please reconnect to the internet and try again.")
    }

    url <- "https://raw.githubusercontent.com/Visorbearer/PFW/main/inst/extdata/pfw_example.csv"
    path <- tempfile(fileext = ".csv")

    tryCatch( # nocov start
      httr2::request(url) |>
        httr2::req_user_agent("PFW R package") |>
        httr2::req_perform(path = path),
      error = function(e) {
        stop("Failed to download example dataset: ", e$message)
      } # nocov end
    )
  }
  data <- read.csv(path)
  attr(data, "pfw_import_path") <- dirname(path)
  message("Example dataset loaded.")
  return(invisible(data))
}
