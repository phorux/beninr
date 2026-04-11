
# beninr <img src="man/figures/logo.png" align="right" height="139" alt="" />

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

**beninr** is an R package that provides curated and harmonized datasets on 
Benin across key domains such as demography, economy, education, climate, 
biodiversity, and environment. In addition, it includes built-in functions 
for data cleaning and exploration.

## Installation

You can install the development version of **beninr** from GitHub with:

``` r
# install.packages("remotes") # if you don't have it yet
remotes::install_github("phorux/beninr")
```

## Example

``` r
library(beninr)

# Load the RGPH4 village-level census dataset
data("RGPH4")

# View the first rows
head(RGPH4)
```

``` r
# Summarize total population by Department
library(dplyr)

RGPH4 %>%
  group_by(Departements) %>%
  summarize(Total_Population = sum(Total, na.rm = TRUE))
```

## Citation

``` r
citation("beninr")
```
