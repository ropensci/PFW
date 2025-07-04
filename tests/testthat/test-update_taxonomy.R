test_that("update_taxonomy downloads and saves a file", {
  skip_on_cran()
  test_taxonomy_folder <- withr::local_tempdir()

  # Set fake user response so it doesn't block
  withr::with_envvar(c(PFW_TEST_RESPONSE = "y"), {
    update_taxonomy(path = test_taxonomy_folder)
  })

  # Check that files were written
  expect_true(file.exists(file.path(test_taxonomy_folder, "PFW_spp_translation_table.csv")))
  expect_true(file.exists(file.path(test_taxonomy_folder, ".last_modified")))
})

test_that("update_taxonomy warns when an existing file is present", {
  test_folder <- withr::local_tempdir()

  # Create a fake translation file and fake timestamp
  existing_file <- file.path(test_folder, "PFW_spp_translation_table.csv")
  write.csv(data.frame(Species = "Song Sparrow"), existing_file, row.names = FALSE)
  writeLines("Fake timestamp", file.path(test_folder, ".last_modified"))

  # Set env var to bypass prompt
  withr::with_envvar(c(PFW_TEST_RESPONSE = "n"), {
    expect_message(
      update_taxonomy(path = test_folder),
      "A species translation table file already exists")
    }
  )
})
