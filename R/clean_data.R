#' @title Clean NOAA Earthquake Data
#'
#' @description \code{eq_clean_data} takes NOAA earthquake dataset and does some data
#'     manipulations on it.
#'
#' @details This function takes NOAA earthquake data and creates a \code{Date} column
#'     of type date by combining \code{Year}, \code{Mo} and \code{Dy}. It then
#'     makes sure that \code{Longitude} and \code{Latitude} are of type numeric.
#'
#' @param data a dataframe of NOAA earthquake.
#'
#' @return A \code{tibble} with a new column \code{Date} but removing
#'     \code{Year}, \code{Mo} & \code{Dy} variables of the input data frame.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' data(NOAA_data)
#' eq_clean_data(NOAA_data)
#'
#' @export
eq_clean_data <- function(data) {
  clean_data <- data %>%
    dplyr::mutate(Year = stringr::str_pad(Year, 4, "left", pad = 0)) %>%
    tidyr::unite(Date, Year, Mo, Dy, remove = TRUE) %>%
    dplyr::mutate(Date = lubridate::ymd(Date),
                  dplyr::across(c("Longitude", "Latitude"), as.numeric))
  clean_data
}


#' @title Clean and Separate NOAA Earthquake Location Field
#'
#' @description This function takes a NOAA earthquake data frame and do some data
#'     manipulation on \code{Locaiton Name}.
#'
#' @details \code{eq_location_clean} function manipulates the \code{Location Name} by
#'     separating country name and location name into \code{Country} and
#'     \code{Location} columns. It then turns \code{Location} into Title Case.
#'
#' @param data a data frame of NOAA earthquake.
#'
#' @importFrom magrittr %>%
#'
#' @return
#' a \code{tibble} with one additional column which is the result of the
#'  previous \code{Location Name} being divided into two columns \code{Country}
#'  and \code{Location}.
#'
#' @examples
#' data(NOAA_data)
#' eq_location_clean(NOAA_data)
#'
#' @export
eq_location_clean <- function(data) {
  NOAA_clean <- data %>%
    tidyr::separate(`Location Name`, c("Country", "Location"), ": ", remove = TRUE) %>%
    dplyr::mutate(Location = stringr::str_to_title(Location),
                  Location = stringr::str_trim(Location, "left"))
  NOAA_clean
}

