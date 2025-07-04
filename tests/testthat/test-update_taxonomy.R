test_that("update_taxonomy downloads and saves a file", {
  skip_on_cran()
  # Create a temporary user_dir
  test_user_dir <- withr::local_tempdir()
  species_dir <- file.path(test_user_dir, "SpeciesTranslationTable")
  taxonomy_path <- file.path(species_dir, "PFW_spp_translation_table.csv")

  # Run the function
  try(update_taxonomy(user_dir = test_user_dir), silent = TRUE)

  # Check that the downloaded file exists
  expect_true(file.exists(taxonomy_path))
})

test_that("update_taxonomy warns when an existing file is present", {
  skip_on_cran()

  # Setup a fake user_dir
  test_user_dir <- withr::local_tempdir()
  species_dir <- file.path(test_user_dir, "SpeciesTranslationTable")
  dir.create(species_dir, recursive = TRUE)

  # Create a fake CSV file
  existing_file <- file.path(species_dir, "PFW_spp_translation_table.csv")
  write.csv(data.frame(Species = "Song Sparrow"), existing_file, row.names = FALSE)

  # Create a fake timestamp file to simulate a previous download
  timestamp_file <- file.path(species_dir, ".last_modified")
  writeLines("Fake-Timestamp", timestamp_file)

  # Simulate non-overwrite response
  withr::with_envvar(list(PFW_TEST_RESPONSE = "n"), {
    expect_message(
      update_taxonomy(user_dir = test_user_dir),
      "A species translation table file already exists."
    )
  })
})
