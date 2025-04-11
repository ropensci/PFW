test_that("pfw_date filters by year correctly", {
  test_data <- data.table::data.table(
    Year = c(2020, 2021, 2022, 2023),
    Month = c(11, 12, 1, 2),
    Day = c(1, 1, 1, 1),
    OTHER = c("a", "b", "c", "d")
  )

  filtered <- pfw_date(test_data, year = 2022:2023)
  expect_true(all(filtered$Year %in% 2022:2023))
})

test_that("pfw_date filters by normal month range correctly", {
  test_data <- data.table::data.table(
    Year = rep(2023, 12),
    Month = 1:12,
    Day = rep(1, 12)
  )

  filtered <- pfw_date(test_data, month = 3:5)
  expect_equal(sort(unique(filtered$Month)), 3:5)
})

test_that("pfw_date filters by wrapped month range (e.g. 11 to 2)", {
  test_data <- data.table::data.table(
    Year = rep(2023, 12),
    Month = 1:12,
    Day = rep(1, 12)
  )

  filtered <- pfw_date(test_data, month = 11:2)
  expect_equal(sort(unique(filtered$Month)), c(1, 2, 11, 12))
})

test_that("pfw_date attaches correct filter attribute", {
  test_data <- data.table::data.table(
    Year = c(2021, 2022),
    Month = c(11, 2),
    Day = c(1, 1)
  )

  filtered <- pfw_date(test_data, year = 2022, month = c(11, 2))

  filters <- attr(filtered, "pfw_filters")
  expect_false(is.null(filters))
  expect_true(any(vapply(filters, function(f) f$type == "date", logical(1))))

  date_filter <- Filter(function(f) f$type == "date", filters)[[1]]
  expect_equal(date_filter$value$year, 2022)
  expect_equal(date_filter$value$month, c(11, 2))
})

test_that("pfw_date errors if data is missing or NULL", {
  expect_error(pfw_date(), "No dataset provided.")
  expect_error(pfw_date(NULL), "No dataset provided.")
})
