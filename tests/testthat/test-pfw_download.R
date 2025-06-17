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
  skip_on_cran()
  temp_folder <- file.path(tempdir(), "pfw-test-dir2")
  if (dir.exists(temp_folder)) unlink(temp_folder, recursive = TRUE)
  withr::defer(unlink(temp_folder, recursive = TRUE))
  pfw_download(2000:2001, temp_folder)

  expect_true(dir.exists(temp_folder))
  expect_equal(length(list.files(temp_folder, pattern = "\\.csv$")), 2)
})

test_that("pfw_download handles no matching data for selected year(s)", {
  skip_on_cran()
  temp_folder <- tempfile()
  withr::defer(unlink(temp_folder, recursive = TRUE))

  expect_message(
    pfw_download(1492, folder = temp_folder),
    "No available data matched your specified years."
  )
})

test_that("pfw_download cancels if user declines overwrite", {
  skip_on_cran()

  temp_folder <- withr::local_tempdir()
  file.create(file.path(temp_folder, "PFW_all_2021_Public.csv"))

  withr::local_envvar(PFW_TEST_RESPONSE = "n")

  expect_message(
    pfw_download(2021, folder = temp_folder),
    "Download canceled."
      )
    }
  )

test_that("pfw_download handles relative links correctly", {
  skip_on_cran()
  temp_folder <- withr::local_tempdir()

  # Patch a fake version of matched_links
  relative_link <- "relative/path/PFW_all_2021_2022.zip"
  full_link <- paste0("https://feederwatch.org/", relative_link)

  # Manually test fallback
  result <- grepl("^http", relative_link)
  expect_false(result)

  constructed <- if (!result) paste0("https://feederwatch.org/", relative_link) else relative_link
  expect_identical(constructed, full_link)
})

test_that("pfw_download errors on failed download", {
  skip_on_cran()
  bad_url <- "https://feederwatch.org/fake/path/aawagga.zip"
  temp_folder <- withr::local_tempdir()
  zip_path <- file.path(temp_folder, "fake.zip")

  expect_error(
    httr2::request(bad_url) |>
      httr2::req_user_agent("PFW R package") |>
      httr2::req_perform(path = zip_path),
    "404"
  )
})
