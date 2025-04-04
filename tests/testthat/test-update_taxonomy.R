test_that("update_taxonomy downloads and saves a file", {
  # Create a temporary directory to mimic inst/extdata/SpeciesTranslationTable/
  test_taxonomy_folder <- tempfile()
  dir.create(test_taxonomy_folder, recursive = TRUE)

  # Manually set the path where the file should be saved
  taxonomy_path <- file.path(test_taxonomy_folder, "PFW_spp_translation_table.csv")

  # Simulate downloading a file by writing fake data
  fake_data <- data.frame(Species = "Jonathan's Grouse")
  write.csv(fake_data, taxonomy_path, row.names = FALSE)

  # Check that the file now exists
  expect_true(file.exists(taxonomy_path))

  # Cleanup: Remove the test folder after testing
  unlink(test_taxonomy_folder, recursive = TRUE)
})

test_that("update_taxonomy warns when an existing file is present", {
  test_folder <- tempfile()
  dir.create(test_folder, recursive = TRUE)

  # Create a dummy existing file
  existing_file <- file.path(test_folder, "PFW_spp_translation_table.csv")
  write.csv(data.frame(Species = "Song Sparrow"), existing_file, row.names = FALSE)

  # Override the translation directory and bypass readline()
  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = test_folder, PFW_TEST_RESPONSE = "n"),
    {
      expect_message(update_taxonomy(), "A species translation table file already exists.")
    }
  )

  # Cleanup
  unlink(test_folder, recursive = TRUE)
})


