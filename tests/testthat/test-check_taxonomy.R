test_that("check_taxonomy detects missing and existing files correctly", {
  # No directory exists
  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = tempfile()),
    {
      expect_message(expect_false(check_taxonomy()),
                     "No species translation table detected")
    }
  )

  # Directory exists but no csv
  test_folder <- tempfile()
  dir.create(test_folder, recursive = TRUE)

  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = test_folder),
    {
      expect_message(expect_false(check_taxonomy()),
                     "No species translation table detected")
    }
  )

  # Now add a dummy .csv
  fake_file <- file.path(test_folder, "test_species_translation.csv")
  write.csv(data.frame(Species = "Jonathan's Grouse"), fake_file, row.names = FALSE)

  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = test_folder),
    {
      expect_true(check_taxonomy())
    }
  )

  unlink(test_folder, recursive = TRUE)
})
