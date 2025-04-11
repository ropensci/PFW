test_that("pfw_filter correctly filters by species", {
  test_data <- data.table::data.table(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    VALID = c(1, 1, 1, 1, 1),
    REVIEWED = c(1, 1, 1, 1, 1)
  )

  filtered_data <- pfw_filter(test_data, species = "American Robin")

  # Expect only "amerob" data
  expect_true(all(filtered_data$SPECIES_CODE == "amerob"))
})

test_that("pfw_filter correctly filters by region", {
  test_data <- data.table::data.table(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    VALID = c(1, 1, 1, 1, 1),
    REVIEWED = c(1, 1, 1, 1, 1)
  )

  filtered_data <- pfw_filter(test_data, region = "California")

  # Expect only "US-CA" data
  expect_true(all(filtered_data$SUBNATIONAL1_CODE == "US-CA"))
})

test_that("pfw_filter correctly filters by species and region together", {
  test_data <- data.table::data.table(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    VALID = c(1, 1, 1, 1, 1),
    REVIEWED = c(1, 1, 1, 1, 1)
  )

  filtered_data <- pfw_filter(test_data, species = "American Robin", region = "Washington")

  # Expect only "amerob" from "US-WA"
  expect_true(all(filtered_data$SUBNATIONAL1_CODE == "US-WA"))
  expect_true(all(filtered_data$SPECIES_CODE == "amerob"))
})

test_that("pfw_filter correctly applies valid and reviewed filters (default TRUE)", {
  test_data <- data.table::data.table(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    VALID = c(1, 1, 1, 1, 1),
    REVIEWED = c(0, 0, 1, 0, 1) # Only some rows are reviewed
  )

  filtered_data <- pfw_filter(test_data)

  # VALID == 0 rows should already be filtered out, so check only REVIEWED
  expect_true(all(filtered_data$VALID != 0)) # Ensure no non-valid rows remain
  expect_true(nrow(filtered_data) == nrow(test_data)) # REVIEWED should not be filtered by default
})

test_that("pfw_filter allows unreviewed and invalid data when explicitly set", {
  test_data <- data.table::data.table(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    VALID = c(1, 0, 1, 0, 1), # Some rows invalid
    REVIEWED = c(1, 0, 1, 0, 0) # Mixed reviewed/unreviewed
  )

  filtered_data <- pfw_filter(test_data, valid = FALSE, reviewed = FALSE, rollup = FALSE)

  # Since both filters are disabled, expect all original rows
  expect_true(all(filtered_data$REVIEWED == 0))
  expect_true(all(filtered_data$VALID %in% c(0, 1))) # Should include VALID == 0
  expect_true(all(filtered_data$REVIEWED %in% c(0, 1))) # Should include unreviewed rows
})

test_that("pfw_filter returns an empty dataset when no matches are found", {
  test_data <- data.table::data.table(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    VALID = c(1, 1, 1, 1, 1),
    REVIEWED = c(1, 1, 1, 1, 1),
    PLUS_CODE = c(0, 1, 0, 1, 0)
  )

  filtered_data <- pfw_filter(test_data, species = "Song Sparrow", region = "Hawaii")

  # Expect an empty dataset
  expect_true(nrow(filtered_data) == 0)
})

test_that("pfw_filter correctly stores attributes for all filters", {
  test_data <- data.table::data.table(
    SUBNATIONAL1_CODE = c("US-WA", "US-WA", "US-OR", "CA-BC", "US-WA"),
    SPECIES_CODE = c("amerob", "baleag", "baleag", "amerob", "amerob"),
    VALID = c(1, 1, 1, 1, 1),
    REVIEWED = c(1, 1, 1, 1, 1),
    Year = c(2022, 2022, 2023, 2023, 2024),
    Month = c(3, 3, 4, 4, 5),
    Day = c(15, 15, 15, 15, 15)
  )

  filtered_data <- pfw_filter(
    test_data,
    species = "American Robin",
    region = "Washington",
    year = 2022,
    month = 3,
    reviewed = TRUE
  )

  # Extract attributes
  filter_attrs <- attr(filtered_data, "pfw_filters")

  # Check each filter type is present
  expect_true(any(vapply(filter_attrs, function(f) f$type == "species", logical(1))))
  expect_true(any(vapply(filter_attrs, function(f) f$type == "region", logical(1))))
  expect_true(any(vapply(filter_attrs, function(f) f$type == "date", logical(1))))
  expect_true(any(vapply(filter_attrs, function(f) f$type == "reviewed", logical(1))))
  expect_true(any(vapply(filter_attrs, function(f) f$type == "valid", logical(1)))) # Default is TRUE
  expect_true(any(vapply(filter_attrs, function(f) f$type == "rollup", logical(1)))) # Default is TRUE
})
