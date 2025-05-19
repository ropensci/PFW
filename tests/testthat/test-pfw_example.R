test_that("pfw_example loads the example dataset if downloaded", {
  skip_on_cran()
  example_data <- pfw_example()
  expect_true(is.data.frame(example_data))
  expect_gt(nrow(example_data), 0)
  expect_true(all(c("SPECIES_CODE", "Year", "Month") %in% names(example_data)))
})

test_that("pfw_example falls back to GitHub download if system file is missing", {
  skip_on_cran()

  example_file <- system.file("extdata", "pfw_example.csv", package = "PFW")
  temp_name <- tempfile(fileext = ".csv")

  # Temporarily rename the file
  file.rename(example_file, temp_name)
  on.exit(file.rename(temp_name, example_file), add = TRUE)

  data <- pfw_example()
  expect_s3_class(data, "data.frame")
  expect_gt(nrow(data), 0)
})
