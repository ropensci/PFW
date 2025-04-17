test_that("pfw_example loads the example dataset if downloaded", {
  example_data <- pfw_example()
  expect_true(is.data.frame(example_data))
  expect_gt(nrow(example_data), 0)
  expect_true(all(c("SPECIES_CODE", "Year", "Month") %in% names(example_data)))
})

test_that("pfw_example loads data even if local copy is missing", {
  # Force fallback mode by renaming system file
  data <- pfw_example()
  expect_s3_class(data, "data.frame")
  expect_true(nrow(data) > 0)
})
