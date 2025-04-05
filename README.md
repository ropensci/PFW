
# PFW: Tools for Filtering and Processing Data from Project FeederWatch <img src="Logo.png" align="right" width=140 alt="Hexagonal PFW logo, featuring a Dark-eyed Junco with a seed in its beak."/>

<!-- badges: start -->
[![Static
Badge](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/Visorbearer/PFW/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Visorbearer/PFW/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`PFW` is an R package designed for easy filtering, preparation, 
and management of data from [Project FeederWatch](https://feederwatch.org/). 
Project FeederWatch is a community-driven project initiated in the 1980s
and run by the [Cornell Lab of Ornithology](https://www.birds.cornell.edu/) and [Birds Canada](https://www.birdscanada.org/)
that compiles bird observations from thousands of backyards, nature centers, 
community areas, and other locales across North America. Included in `PFW`
are tools for taxonomic rollup, filtering by survey characteristics 
(species, state, etc.), merging in site metadata, and zerofilling for presence/absence modeling.

## Installation

`PFW` is still in development, but the development version can be installed from GitHub using:

``` r
install.packages("devtools")

# Install this package
devtools::install_github("Visorbearer/PFW")
```

## Vignette

Background and details on using `PFW` to filter and process Project FeederWatch data are outlined in the [vignette](https://github.com/Visorbearer/PFW/blob/main/vignettes/PFW.Rmd). <!-- Replace this link after running pkgdown -->

## Citation

To cite `PFW` in publications, use:

> Maron, M. (2025). PFW: Tools for Filtering and Processing Data from Project FeederWatch.  
> R package version 0.0.1. https://github.com/Visorbearer/PFW

You can also run

```r
citation("PFW")
```

in R when `PFW` is loaded.

## Quick Start

This is a simple example which shows you how to do basic Project FeederWatch importing, filtering, zerofilling, and site data attachment.
Here, we'll load the example dataset and filter it for Song Sparrow, Dark-eyed Junco, and Spotted Towhee from Washington
and Oregon between 2022 and 2024 in November-February. Then, we'll zerofill that data and attach our site metadata:

``` r
library(PFW)

# Load in the included example dataset
data <- pfw_example() # If you were using your own data, this would be pfw_import() instead.
# pfw_import() defaults to the built-in filepath "/data-raw", but will accept a different filepath.

# Create a list of study species
species_list <- c("Song Sparrow", "Dark-eyed Junco", "Spotted Towhee")

# Create a list of study regions
region_list <- c("Washington", "Oregon")

# Filter data by species and region, from 2022–2024 during November-February
data_filtered <- pfw_filter(data,
  region = region_list,
  species = species_list,
  year = 2022:2024,
  month = 11:2, # pfw_date(), which is called within pfw_filter(), 
                #will appropriately wrap this around the end of the year.
  valid = TRUE, # TRUE by default
  rollup = TRUE # TRUE by default
)

# View the filters that were applied
pfw_attr(data_filtered)

# Zerofill missing species/survey instance combos
data_zf <- pfw_zerofill(data_filtered)

# Attach site description metadata
# Replace "path/sitedata.csv" with the actual path to the downloaded file
data_full <- pfw_sitedata(data_zf, path = "path/sitedata.csv")
# If the file does not exist at that path, pfw_sitedata will
# download it there from the Project FeederWatch website.
# Alternatively, you can manually download the site description file from:
# https://feederwatch.org/explore/raw-dataset-requests/
```
## Usage

As a quick and easy-to-reference guide to a standard `PFW` workflow, here's
a brief list of the package's functions and use:

### Downloading and Importing

- `pfw_download()`: download raw Project FeederWatch data from the website.
- `pfw_import()`: import downloaded raw data into the R project, optionally filtering it.
- `pfw_example()`: download and/or load the example raw Project FeederWatch dataset.
- `update_taxonomy()`: updates the taxonomy in the species translation table.
- `pfw_sitedata()`: imports site metadata and attaches it to your selected dataset.

### Filtering and Zerofilling

- `pfw_filter()`: filters your data by region, species, date, and reviewed/valid status. Also applies taxonomic rollup.
  Nested within `pfw_filter()` are several functions which can be run independently:
  - `pfw_region()`: filters your data by region (state/province or whole country).
  - `pfw_species()`: filters your data by species.
  - `pfw_date()`: filters your data by year and month.
  - `pfw_rollup()`: applies taxonomic rollup to your data.

- `pfw_truncate()`: optionally limits your data only to days all Project FeederWatch years have run.
- `pfw_zerofill()`: zerofills your data for all species in it.

### Helper functions

- `pfw_attr()`: lists all filters applied to your data.
- `pfw_dictionary()`: calls variable definitions and descriptions from the data dictionary.

## Acknowledgements

`PFW` was built from code originally developed for Project FeederWatch data preparation in Maron et al. (2025). 
While the function scripts in this package were created specifically for PFW, the code they are based on 
benefited greatly from examples and code snippets provided by Emma Greig, who passed away prior to the 
package's creation.
