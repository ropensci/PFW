test_that("pfw_sitedata correctly merges site metadata", {
  # Sample observation data
  obs_data <- data.frame(
    LOC_ID = c("L001", "L002"),
    PROJ_PERIOD_ID = c(1001, 1002),
    SPECIES_CODE = c("amerob", "bkcchi"),
    stringsAsFactors = FALSE
  )

  # Sample site metadata
  site_data <- data.frame(
    loc_id = c("L001", "L002"),
    proj_period_id = c(1001, 1002),
    habitat_type = c("urban", "rural"),
    feeder_count = c(2, 1),
    stringsAsFactors = FALSE
  )

  # Save temp CSV
  temp_path <- tempfile(fileext = ".csv")
  write.csv(site_data, temp_path, row.names = FALSE)

  # Run merge
  merged <- pfw_sitedata(obs_data, temp_path)

  # Test merged output
  expect_equal(nrow(merged), 2)
  expect_true(all(c("habitat_type", "feeder_count") %in% colnames(merged)))
  expect_equal(merged$habitat_type[1], "urban")
  expect_equal(merged$feeder_count[2], 1)
})

test_that("pfw_sitedata throws error for valid path but bad data", {
  path <- file.path(tempdir(), "CoolBugsISawLastMonday.csv")

  expect_error(
    pfw_sitedata(data.frame(), path = path),
    "must include LOC_ID and PROJ_PERIOD_ID"
  )

  if (file.exists(path)) {
    unlink(path)
  }
})

test_that("pfw_sitedata throws error with missing columns", {
  bad_data <- data.frame(ID = 1:2)
  temp_path <- tempfile(fileext = ".csv")
  write.csv(data.frame(loc_id = "L001", proj_period_id = 1001), temp_path, row.names = FALSE)

  expect_error(pfw_sitedata(bad_data, path = temp_path), "must include LOC_ID and PROJ_PERIOD_ID")
})
