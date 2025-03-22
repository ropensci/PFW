#' Celebrate!
#'
#' This function is just if you want to celebrate finishing some particularly difficult code.
#' Have a little fun with it-- I used it to celebrate finishing this R package!
#'
#' @param name (Optional) Your name, if you want!
#' @return A celebration!
#' @export

.celebrate <- function(name = NULL) {
  if (!is.null(name)) {
    print(paste0("Congratulations ", name, ", you did it!"))
  } else {
    print("Congratulations, you did it!")
  }
}
