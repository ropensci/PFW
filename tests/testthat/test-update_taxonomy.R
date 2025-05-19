test_that("update_taxonomy downloads and saves a file", {
  skip_on_cran()
  # Create a temporary directory to mimic inst/extdata/SpeciesTranslationTable/
  test_taxonomy_folder <- withr::local_tempdir()

  # Manually set the path where the file should be saved
  taxonomy_path <- file.path(test_taxonomy_folder, "PFW_spp_translation_table.csv")

  # Simulate downloading a file by writing fake data
  fake_data <- data.frame(Species = "Jonathan's Grouse")
  write.csv(fake_data, taxonomy_path, row.names = FALSE)

  # Check that the file now exists
  expect_true(file.exists(taxonomy_path))
})

test_that("update_taxonomy warns when an existing file is present", {
  test_folder <- withr::local_tempdir()

  # Create a fake file
  existing_file <- file.path(test_folder, "PFW_spp_translation_table.csv")
  write.csv(data.frame(Species = "Song Sparrow"), existing_file, row.names = FALSE)

  # Override the translation directory and bypass readline()
  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = test_folder, PFW_TEST_RESPONSE = "n"),
    {
      expect_message(update_taxonomy(), "A species translation table file already exists.")
    }
  )
})
