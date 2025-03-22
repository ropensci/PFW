test_that("pfw_truncate filters correctly and preserves structure", {
  # Sample data covering before, during, and after the PFW window
  test_data <- data.frame(
    Year = c(2023, 2023, 2023, 2023),
    Month = c(10, 11, 3, 4),
    Day = c(31, 9, 15, 4),
    HOW_MANY = c(1, 2, 3, 4)
  )

  truncated <- pfw_truncate(test_data)

  # Expect only 2 records (Nov 9 and Mar 15)
  expect_equal(nrow(truncated), 2)

  # Expect only the "in-window" dates to remain
  expect_true(all(truncated$Month %in% c(11, 3)))

  # Check that extra columns are preserved
  expect_true("HOW_MANY" %in% names(truncated))

  # Check that DOY/date temp columns were removed
  expect_false(".PFW_DOY" %in% names(truncated))
  expect_false(".PFW_DATE_TEMP" %in% names(truncated))
})
