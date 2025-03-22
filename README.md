
# PFW

<!-- badges: start -->
<!-- badges: end -->

`PFW` is an R package designed for easy filtering, preparation, and management of data from [Project FeederWatch](https://feederwatch.org/).  
Included are tools for taxonomic rollup, filtering by survey characteristics (species, state, etc.), merging in site metadata, and zerofilling for presence/absence modeling.

## Installation

`PFW` is still in development, but the development version can be installed from GitHub using:

``` r
install.packages("devtools")

# Install this package
devtools::install_github("Visorbearer/PFW")
```

## Example

This is an example which shows you how to do basic Project FeederWatch importing, filtering, zerofilling, and site data attachment:

``` r
library(PFW)

# Load in the included example dataset
data <- pfw_example() #If you were using your own data, this would be pfw_import() instead.
#pfw_import() defaults to the built-in filepath "/data-raw", but will accept a different filepath.

#Create a list of study species
species_list <- c("Song Sparrow", "Dark-eyed Junco", "Spotted Towhee")

#Create a list of study regions
region_list <- c("Washington", "Oregon")

# Filter data by species and region, from 2022–2024 during November-February
data_filtered <- pfw_filter(data,
  region = region_list,
  species = species_list,
  year = 2022:2024,
  month = 11:2, #pfw_date(), which is called within pfw_filter(), will appropriately wrap this around the end of the year.
  valid = TRUE, #TRUE by default
  rollup = TRUE #TRUE by default
)

# View the filters that were applied
pfw_attr(data_filtered)

# Zerofill missing species/checklist combos
data_zf <- pfw_zerofill(data_filtered)

# Attach site description metadata
# Replace "path/to/site_data.csv" with the actual path to the downloaded file
data_full <- pfw_sitedata(data_zf, path = "path/to/site_data.csv")
# You can download the site description file from:
# https://feederwatch.org/explore/raw-dataset-requests/
```
