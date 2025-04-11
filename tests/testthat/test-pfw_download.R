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

test_that("pfw_download handles no matching data for selected year(s)", {
  temp_folder <- tempfile()
  withr::defer(unlink(temp_folder, recursive = TRUE))

  expect_message(
    pfw_download(1492, folder = temp_folder),
    "No available data matched your specified years."
  )
})

test_that("pfw_download cancels if user declines overwrite", {
  temp_folder <- tempfile()
  dir.create(temp_folder)
  file.create(file.path(temp_folder, "existing.csv"))

  withr::defer(unlink(temp_folder, recursive = TRUE))

  # Pretend user said no
  withr::with_envvar(
    c(PFW_TEST_RESPONSE = "n"),
    {
      expect_message(
        pfw_download(2021, folder = temp_folder),
        "Download canceled."
      )
    }
  )
})
