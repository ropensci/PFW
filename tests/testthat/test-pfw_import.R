test_that("pfw_import reads CSV files correctly", {
  # Create a temporary test folder
  test_folder <- withr::local_tempdir()

  # Create two valid dummy PFW-style CSVs
  write.csv(data.frame(
    SPECIES_CODE = c("amerob"),
    HOW_MANY = 10,
    SUB_ID = "S001"
  ), file.path(test_folder, "test1.csv"), row.names = FALSE)

  write.csv(data.frame(
    SPECIES_CODE = c("norcar"),
    HOW_MANY = 5,
    SUB_ID = "S002"
  ), file.path(test_folder, "test2.csv"), row.names = FALSE)

  # Run pfw_import()
  imported_data <- pfw_import(folder = test_folder)

  # Assert success
  expect_equal(nrow(imported_data), 2)
  expect_true(all(c("SPECIES_CODE", "HOW_MANY", "SUB_ID") %in% names(imported_data)))
})

test_that("pfw_import skips files with incorrect structure", {
  test_folder <- withr::local_tempdir()

  # Valid file
  write.csv(data.frame(
    SPECIES_CODE = "amerob", HOW_MANY = 1, SUB_ID = "S1"
  ), file.path(test_folder, "valid.csv"), row.names = FALSE)

  # Invalid file
  write.csv(data.frame(SPECIES_CODE = "aaaah!"),
            file.path(test_folder, "invalid.csv"), row.names = FALSE)

  imported <- pfw_import(folder = test_folder)

  expect_equal(nrow(imported), 1)
  expect_equal(imported$SPECIES_CODE, "amerob")
})

test_that("pfw_import applies filters and stores attributes correctly", {
  # Create a temporary folder with a fake PFW-style file
  test_folder <- withr::local_tempdir()

  test_data <- data.frame(
    SUB_ID = c("S1", "S2", "S3"),
    LOC_ID = c("L1", "L2", "L3"),
    SUBNATIONAL1_CODE = c("US-WA", "US-OR", "US-WA"),
    SPECIES_CODE = c("amerob", "norcar", "amerob"),
    HOW_MANY = c(5, 3, 6),
    VALID = c(1, 1, 1),
    REVIEWED = c(1, 1, 0),
    Year = c(2022, 2022, 2023),
    Month = c(3, 4, 11),
    Day = c(10, 15, 20)
  )

  # Save as CSV to mimic user data
  write.csv(test_data, file.path(test_folder, "pfw_test.csv"), row.names = FALSE)

  # Run import with filters applied
  imported <- pfw_import(
    folder = test_folder,
    species = "American Robin",
    region = "Washington",
    year = 2022
  )

  # Confirm filtered content
  expect_true(nrow(imported) == 1)
  expect_true(all(imported$SPECIES_CODE == "amerob"))

  # Check filter attributes
  filters <- attr(imported, "pfw_filters")
  expect_true(any(vapply(filters, function(f) f$type == "species", logical(1))))
  expect_true(any(vapply(filters, function(f) f$type == "region", logical(1))))
  expect_true(any(vapply(filters, function(f) f$type == "date", logical(1))))
  expect_true(any(vapply(filters, function(f) f$type == "valid", logical(1))))
})
