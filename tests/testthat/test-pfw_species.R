test_that("pfw_species filters data correctly by common name", {
  # Create a test dataset
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    COUNT = c(5, 3, 2, 8, 6)
  )

  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = file.path("inst/extdata/SpeciesTranslationTable")),
    {
      filtered_data <- pfw_species(test_data, "American Robin")

      # Expect only "amerob" data
      expect_true(all(filtered_data$SPECIES_CODE == "amerob"))
    }
  )
})

test_that("pfw_species filters data correctly by scientific name", {
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    COUNT = c(5, 3, 2, 8, 6)
  )

  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = file.path("inst/extdata/SpeciesTranslationTable")),
    {
      filtered_data <- pfw_species(test_data, "Turdus migratorius")

      # Expect only "amerob" data
      expect_true(all(filtered_data$SPECIES_CODE == "amerob"))
    }
  )
})

test_that("pfw_species filters multiple species correctly", {
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    COUNT = c(5, 3, 2, 8, 6)
  )

  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = file.path("inst/extdata/SpeciesTranslationTable")),
    {
      filtered_data <- pfw_species(test_data, c("American Robin", "Bald Eagle"))

      # Expect only "amerob" and "baleag" data
      expect_true(all(filtered_data$SPECIES_CODE %in% c("amerob", "baleag")))
    }
  )
})

test_that("pfw_species warns for invalid species", {
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    COUNT = c(5, 3, 2, 8, 6)
  )

  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = file.path("inst/extdata/SpeciesTranslationTable")),
    {
      expect_warning(
        pfw_species(test_data, c("Bubinga's Jiggety", "Northern Cardinal")),
        "The following species were not found"
      )
    }
  )
})
test_that("pfw_species adds correct filter attribute", {
  test_data <- data.frame(
    SUBNATIONAL1_CODE = c("US-WA", "US-CA", "US-OR", "CA-BC", "US-TX"),
    SPECIES_CODE = c("amerob", "norcar", "baleag", "amerob", "norcar"),
    COUNT = c(5, 3, 2, 8, 6)
  )

  withr::with_envvar(
    list(PFW_TRANSLATION_DIR = file.path("inst/extdata/SpeciesTranslationTable")),
    {
      filtered_data <- pfw_species(test_data, c("American Robin", "Bald Eagle"))

      filters <- attr(filtered_data, "pfw_filters")

      expect_false(is.null(filters))
      expect_true(any(vapply(filters, function(f) f$type == "species", logical(1))))

      species_filter <- Filter(function(f) f$type == "species", filters)[[1]]
      expect_equal(tolower(sort(species_filter$value)), tolower(sort(c("American Robin", "Bald Eagle"))))
    }
  )
})
