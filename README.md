# Documentation

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# noaar

<!-- badges: start -->
[![Build Status](https://travis-ci.org/sheilaNAA/noaar.svg?branch=main)](https://travis-ci.org/sheilaNAA/noaar)
<!-- badges: end -->

The `noaar` package has been built for cleaning and mapping NOAA's Significant 
Earthquake data. 

## Installation

In order to install the `noaar` package, you must first install the
`devtools` package And the development version from 
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sheilaNAA/noaar")
```
## Load and clean package data

For loading the `NOAA_data` you can use `noaar::NOAA_data`, then you can 
call `eq_clean_data` and `eq_location_clean` to do some modifications on a 
number of variables. 

```{r warning=FALSE}
library(noaar)
library(magrittr)
NOAA_data <- noaar::NOAA_data
NOAA_data <- NOAA_data %>%
  eq_clean_data() %>%
  eq_location_clean()
```

Now data is ready to be used for visualization and mapping.
