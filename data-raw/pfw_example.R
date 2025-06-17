## Script to import the example data
# Originally, this came from a backup GitHub file, but no more... as long as it works :)

tmp <- tempdir()
pfw_download(years = 2021:2024, folder = tmp)
pfw_example <- pfw_import(tmp, region = c("US-WA", "US-OR"))

usethis::use_data(pfw_example, overwrite = TRUE, compress = "xz")

# File size check for development
# tools::checkRdaFiles("data/pfw_example.rda")
