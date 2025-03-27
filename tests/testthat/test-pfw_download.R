test_that("pfw_download requires input years", {
  expect_error(pfw_download(), "You must specify at least one year.")
  expect_error(pfw_download(NULL), "You must specify at least one year.")
})

test_that("pfw_download creates folder if it doesn't exist", {
  temp_folder <- file.path(tempdir(), "pfw-test-dir1")
  if (dir.exists(temp_folder)) unlink(temp_folder, recursive = TRUE) # Need this in case next line didn't work
  withr::defer(unlink(temp_folder, recursive = TRUE)) # Need this so that it doesn't keep the files after testing
  if (dir.exists(temp_folder)) unlink(temp_folder, recursive = TRUE)
  expect_false(dir.exists(temp_folder))

  suppressMessages(pfw_download(2021, folder = temp_folder))
  expect_true(dir.exists(temp_folder))
})

test_that("pfw_download downloads multiple files if years spanning multiple are called", {
  temp_folder <- file.path(tempdir(), "pfw-test-dir2")
  if (dir.exists(temp_folder)) unlink(temp_folder, recursive = TRUE)
  withr::defer(unlink(temp_folder, recursive = TRUE))
  pfw_download(2000:2001, temp_folder)

  expect_true(dir.exists(temp_folder))
  expect_equal(length(list.files(temp_folder, pattern = "\\.csv$")), 2)
})
