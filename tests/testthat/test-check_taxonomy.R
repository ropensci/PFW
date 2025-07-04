test_that("check_taxonomy detects missing and existing files correctly", {
  # No directory exists
  nonexistent_dir <- tempfile()
  expect_error(
    check_taxonomy(path = nonexistent_dir),
    "No species translation table detected"
  )

  # Directory exists but no csv
  empty_dir <- withr::local_tempdir()
  expect_error(
    check_taxonomy(path = empty_dir),
    "No species translation table detected"
  )

  # Now add a dummy .csv
  csv_dir <- withr::local_tempdir()
  write.csv(data.frame(Species = "Jonathan's Grouse"), file.path(csv_dir,
                       "test_species_translation.csv"), row.names = FALSE)
  expect_true(check_taxonomy(path = csv_dir))
})
