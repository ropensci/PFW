#' Load Example Project FeederWatch Dataset
#'
#' Provides a sample dataset for demonstration and testing purposes.
#' This dataset includes data from 2021 - May 2024 from Washington, Oregon, and California.
#'
#' @return A data.frame of example PFW data.
#' @export
pfw_example <- function() {
  path <- system.file("extdata", "pfw_example.csv", package = "PFW")
  if (path == "") {
    stop("Example data file not found. Updating the package should fix this!")
  }
  data <- read.csv(path, stringsAsFactors = FALSE)
  attr(data, "pfw_import_path") <- dirname(path)
  message("Example dataset loaded: ", nrow(data), " rows.")
  return(data)
}
