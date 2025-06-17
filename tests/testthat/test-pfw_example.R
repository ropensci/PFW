test_that("pfw_example dataset loads correctly", {
  skip_on_cran()

  data("pfw_example", package = "PFW")
  expect_true(is.data.frame(pfw_example))
  expect_gt(nrow(pfw_example), 0)
  expect_true(all(c("SPECIES_CODE", "Year", "Month") %in% names(pfw_example)))
})
