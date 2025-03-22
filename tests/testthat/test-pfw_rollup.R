test_that("pfw_rollup demotes subspecies correctly", {
  test_data <- data.table::data.table(
    SPECIES_CODE = c("cacgoo2", "frsgro1", "greprc1", "amerob", "cangoo"),
    HOW_MANY = c(5, 3, 2, 8, 6)
  )

  rolled_data <- pfw_rollup(test_data)

  expect_true(all(c("cacgoo1", "sprgro", "grpchi", "amerob", "cangoo") %in% rolled_data$SPECIES_CODE))
  expect_false(any(c("cacgoo2", "frsgro1", "greprc1") %in% rolled_data$SPECIES_CODE))
})

test_that("pfw_rollup removes spuhs", {
  test_data <- data.table::data.table(
    SPECIES_CODE = c("cacgoo2", "frsgro1", "greprc1", "sapsuc", "amerob"),
    HOW_MANY = c(5, 3, 2, 10, 6)
  )

  rolled_data <- pfw_rollup(test_data)

  expect_false("sapsuc" %in% rolled_data$SPECIES_CODE)
  expect_true(all(c("cacgoo1", "sprgro", "grpchi", "amerob") %in% rolled_data$SPECIES_CODE))
})

test_that("pfw_rollup removes hybrids and slashes", {
  test_data <- data.table::data.table(
    SPECIES_CODE = c("cacgoo2", "frsgro1", "greprc1", "x00712", "y00019", "amerob"),
    HOW_MANY = c(5, 3, 2, 8, 9, 6)
  )

  rolled_data <- pfw_rollup(test_data)

  expect_false(any(c("x00712", "y00019") %in% rolled_data$SPECIES_CODE))
  expect_true(all(c("cacgoo1", "sprgro", "grpchi", "amerob") %in% rolled_data$SPECIES_CODE))
})

test_that("pfw_rollup correctly attaches rollup attribute", {
  test_data <- data.table::data.table(
    SPECIES_CODE = c("amerob", "cacgoo2"),
    HOW_MANY = c(5, 3)
  )

  rolled_data <- pfw_rollup(test_data)

  rollup_attr <- attr(rolled_data, "pfw_filters")
  rollup_filter <- purrr::keep(rollup_attr, ~ .x$type == "rollup")

  expect_true(length(rollup_filter) > 0)
  expect_true(rollup_filter[[1]]$value)
})
