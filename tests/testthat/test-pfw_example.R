test_that("pfw_example loads the example dataset", {
  example_data <- pfw_example()
  expect_true(is.data.frame(example_data))
  expect_gt(nrow(example_data), 0)
  expect_true(all(c("SPECIES_CODE", "Year", "Month") %in% names(example_data)))
})
