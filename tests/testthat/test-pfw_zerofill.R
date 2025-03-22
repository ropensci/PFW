test_that("pfw_zerofill adds zeros for undetected species on existing checklists", {
  # Simulated full dataset (like from pfw_import)
  full_data <- data.table::data.table(
    SUB_ID = c("S1", "S1", "S2", "S3"),
    SPECIES_CODE = c("amerob", "baleag", "amerob", "norcar"),
    HOW_MANY = c(2, 1, 3, 1),
    VALID = c(1, 1, 1, 1),
    REVIEWED = c(1, 1, 1, 1)
  )

  # Set the global full_data for zerofill to pull from
  .pfw_env$full_data <- full_data

  # Simulate filtering to two species with attached filters
  filtered_data <- full_data[full_data$SPECIES_CODE %in% c("amerob", "norcar"), ]
  attr(filtered_data, "pfw_filters") <- list(
    list(type = "valid", value = TRUE),
    list(type = "reviewed", value = TRUE),
    list(type = "species", value = c("american robin", "northern cardinal"))
  )

  # Run zerofill
  result <- pfw_zerofill(filtered_data)

  # Expectations
  expect_true(nrow(result) > nrow(filtered_data))
  expect_true(any(result$HOW_MANY == 0))

  # Check for expected zero-filled cases
  expect_true("S2" %in% result$SUB_ID[result$HOW_MANY == 0]) # S2 missing norcar
  expect_true("S3" %in% result$SUB_ID[result$HOW_MANY == 0]) # S3 missing amerob

  # Total combinations = unique SUB_IDs * species
  expected_rows <- length(unique(full_data$SUB_ID)) * length(unique(filtered_data$SPECIES_CODE))
  expect_equal(nrow(result), expected_rows)
})

test_that("pfw_zerofill works with unfiltered data", {
  unfiltered <- data.table::data.table(
    SUB_ID = c("S1", "S2"),
    SPECIES_CODE = c("amerob", "norcar"),
    HOW_MANY = c(3, 2)
  )

  .pfw_env$full_data <- unfiltered

  result <- pfw_zerofill(unfiltered)

  expect_true(nrow(result) > nrow(unfiltered))
  expect_true(any(result$HOW_MANY == 0))
  expect_equal(length(unique(result$SUB_ID)), 2)
  expect_equal(length(unique(result$SPECIES_CODE)), 2)
})
