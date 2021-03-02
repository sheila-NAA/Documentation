#' @title
#' NOAA Significant Earthquake Database
#'
#' @description
#' This dataset was obtained from U.S. National Oceanographic and Atmospheric
#'  Administration (NOAA) on significant earthquakes around the world. It
#'  contains information about 5,933 earthquakes over an
#'  approximately 4,000 year time span.
#'
#' @format
#' A data frame of 6219 observations and 39 variables.
#'
#' @source
#'  NOAA National Centers for Environmental Information
#'  National Geophysical Data Center / World Data Service (NGDC/WDS):
#'  NCEI/WDS Global Significant Earthquake Database. NOAA National Centers
#'  for Environmental Information.
#'  doi:10.7289/V5TD9V7K
#'  \url{https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search}
"NOAA_data"


NOAA_data <- readr::read_delim("inst/extdata/earthquakes.tsv.txt",
                               delim = "\t", col_names = TRUE,
                               col_types = readr::
                                 cols_only(Year = readr::col_double(),
                                           Mo = readr::col_double(),
                                           Dy = readr::col_double(),
                                           `Location Name` = readr::col_character(),
                                           Latitude = readr::col_double(),
                                           Longitude = readr::col_double(),
                                           Mag = readr::col_double(),
                                           Deaths = readr::col_double()),
                               na = c("", "NA"))
