test_that("pfw_dictionary prints correct output for specific variable", {
  # Should include the variable name in output
  expect_message(pfw_dictionary("LOC_ID"), regexp = "LOC_ID")

  # Should include Definition and Description in output
  expect_message(pfw_dictionary("LOC_ID"), regexp = "Definition:")
  expect_message(pfw_dictionary("LOC_ID"), regexp = "Description:")
})

test_that("pfw_dictionary warns when variable is not found", {
  expect_message(
    pfw_dictionary("GENERAL_ODOR"),
    regexp = "No matching variable found"
  )
})

test_that("pfw_dictionary prints all variables when run without argument", {
  expect_message(pfw_dictionary(), regexp = "Variable:")
  expect_message(pfw_dictionary(), regexp = "Definition:")
  expect_message(pfw_dictionary(), regexp = "Description:")
})
