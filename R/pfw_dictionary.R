#' Look Up Definitions from the Project FeederWatch Data Dictionary
#'
#' This function helps users explore the FeederWatch dataset by
#' viewing the full data dictionary or searching for
#' definitions for specific variables.
#'
#' @param variable (Optional) A variable name (e.g., "LOC_ID") to look up. If NULL, prints the full dictionary.
#'
#' @return A printed description (for a variable) or the full dictionary.
#' @examples
#' # View the whole data dictionary
#' pfw_dictionary()
#'
#' # View the data dictionary entry for location ID ("LOC_ID")
#' pfw_dictionary("LOC_ID")
#'
#' @export
pfw_dictionary <- function(variable = NULL) {
  # Locate dictionary file
  dict_path <- list.files(
    path = system.file("extdata", package = "PFW"),
    pattern = "FeederWatch_Data_Dictionary.csv",
    full.names = TRUE
  )

  if (length(dict_path) == 0) {
    stop("Data dictionary not found. Please update the package to reinstall it.")
  }

  # Load the dictionary
  dictionary <- read.csv(dict_path[1], stringsAsFactors = FALSE)

  if (is.null(variable)) {
    for (i in seq_len(nrow(dictionary))) {
      row <- dictionary[i, ]
      if (all(is.na(row)) || all(trimws(row) == "")) next

      cat("\033[1mVariable:\033[0m   ", row$Variable_name, "\n")
      cat("\033[1mDefinition:\033[0m ", row$Definition, "\n")
      cat("\033[1mDescription:\033[0m", row$Description, "\n\n")
    }
    return(invisible(dictionary))
  } else {
    variable <- toupper(variable)
    text_match <- dictionary[dictionary$Variable_name == variable, ]

    if (nrow(text_match) == 0) {
      message("No matching variable found in the dictionary.")
    } else {
      cat("\033[1mVariable:\033[0m   ", text_match$Variable_name, "\n")
      cat("\033[1mDefinition:\033[0m ", text_match$Definition, "\n")
      cat("\033[1mDescription:\033[0m", text_match$Description, "\n")
    }
  }
}
