test_that("check_taxonomy detects missing and existing files correctly", {
  # Make sure it returns FALSE when no taxonomy file exists
  expect_false(check_taxonomy())

  # Create a temporary folder to mimic the SpeciesTranslationTable location
  test_folder <- tempfile()
  dir.create(test_folder, recursive = TRUE)

  # Create a dummy species translation table file
  fake_file <- file.path(test_folder, "test_species_translation.csv")
  write.csv(data.frame(Species = "Jonathan's Grouse"), fake_file, row.names = FALSE)

  # Override the translation directory so check_taxonomy() looks in our test folder
  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = test_folder),
    {
      expect_true(check_taxonomy())
    }
  )

  # Cleanup: Remove the test file
  unlink(test_folder, recursive = TRUE)
})
