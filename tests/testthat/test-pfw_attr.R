test_that("pfw_attr returns filter attributes correctly", {
  test_data <- data.table::data.table(
    SPECIES_CODE = c("amerob", "norcar"),
    SUBNATIONAL1_CODE = c("US-WA", "US-CA"),
    Year = c(2022, 2023),
    Month = c(11, 2),
    Day = c(15, 15),
    VALID = c(1, 1),
    REVIEWED = c(1, 0)
  )

  # Add some fake filters manually
  attr(test_data, "pfw_filters") <- list(
    list(type = "species", value = "american robin"),
    list(type = "region", value = "Washington"),
    list(type = "date", value = list(year = 2022:2023, month = c(11, 12, 1, 2))),
    list(type = "rollup", value = TRUE)
  )

  result <- pfw_attr(test_data)

  # Check result is a list and has expected structure
  expect_type(result, "list")
  expect_true(all(sapply(result, function(x) all(c("type", "value") %in% names(x)))))
  expect_true(any(sapply(result, function(x) x$type == "species")))
  expect_true(any(sapply(result, function(x) x$type == "date")))
})
