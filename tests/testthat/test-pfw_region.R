test_that("pfw_region filters data correctly by state", {
  # Create a fake dataset
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-ON", "US-TX"),
    Species = c("Pullman Sparrow", "Greater Roadrunner", "Jonathan's Grouse", "Zane's Camera-Warbler", "Kickapoo Cavernbird")
  )

  # Filter for Washington
  region_filtered_data <- pfw_region(test_data, "Washington")

  # Expect only Washington (US-WA) data
  expect_true(all(region_filtered_data$SUBNATIONAL1_CODE == "US-WA"))
})

test_that("pfw_region filters data correctly by multiple states", {
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-ON", "US-TX"),
    Species = c("Pullman Sparrow", "Greater Roadrunner", "Jonathan's Grouse", "Zane's Camera-Warbler", "Kickapoo Cavernbird")
  )

  # Filter for Washington and Oregon
  region_filtered_data <- pfw_region(test_data, c("Washington", "Oregon"))

  # Expect only US-WA and US-OR data
  expect_true(all(region_filtered_data$SUBNATIONAL1_CODE %in% c("US-WA", "US-OR")))
})

test_that("pfw_region filters data correctly by country", {
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "CA-ON"),
    Species = c("Pullman Sparrow", "Greater Roadrunner", "Jonathan's Grouse", "Great White Northern Mockingbird", "Zane's Camera-Warbler")
  )

  # Filter for United States
  region_filtered_data <- pfw_region(test_data, "United States")

  # Expect only US-XX data
  expect_true(all(grepl("^US-", region_filtered_data$SUBNATIONAL1_CODE)))
})

test_that("pfw_region errors if regions is not a character vector", {
  test_data <- data.frame(SUBNATIONAL1_CODE = "US-WA")
  expect_error(pfw_region(test_data, 11), "Regions must be a character vector")
})

test_that("pfw_region filters data correctly by Canada", {
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "CA-ON"),
    Species = c("Pullman Sparrow", "Greater Roadrunner", "Jonathan's Grouse", "Great White Northern Mockingbird", "Zane's Camera-Warbler")
  )

  # Filter for Canada
  region_filtered_data <- pfw_region(test_data, "Canada")

  # Expect only CA-XX data
  expect_true(all(grepl("^CA-", region_filtered_data$SUBNATIONAL1_CODE)))
})

test_that("pfw_region returns an error for an invalid region", {
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "CA-ON"),
    Species = c("Pullman Sparrow", "Greater Roadrunner", "Jonathan's Grouse", "Great White Northern Mockingbird", "Zane's Camera-Warbler")
  )

  # Expect an error when filtering by a non-existent region
  expect_error(pfw_region(test_data, "Your Mom's House"), "No matching regions found")
})
test_that("pfw_region adds correct filter attributes", {
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR"),
    Species = c("Species A", "Species B", "Species C")
  )

  region_filtered <- pfw_region(test_data, c("Washington", "Oregon"))

  filters <- attr(region_filtered, "pfw_filters")

  expect_false(is.null(filters))
  expect_true(any(vapply(filters, function(f) f$type == "region", logical(1))))

  region_filter <- Filter(function(f) f$type == "region", filters)[[1]]
  expect_equal(sort(region_filter$value), sort(c("Washington", "Oregon")))
})
