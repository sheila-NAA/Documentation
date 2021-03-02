#' @title Leaflet Map Tools
#'
#' @description \code{eq_map} creates a leaflet map
#'     showing the location of each earthquakes on the map.
#'
#' @details The function maps the epicenters \code{Longitude} and
#'  \code{Latitude} and annotates each point with in popup window containing
#'  annotation data stored in a column of the data frame. The use can choose
#'  which column to be used for popup.
#'
#' @param data The \code{\link{NOAA_data}} data frame.
#' @param annot_col The name of the column to be used for annotation and it
#'  has to be a character strings of length one.
#'
#' @return Returns a leaflet map.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' NOAA_data <- NOAA_data %>%
#' eq_clean_data() %>%
#' eq_location_clean() %>%
#' dplyr::filter(Country == "MEXICO" & lubridate::year(Date) >= 2000)
#' eq_map(data = NOAA_data, annot_col = "Date")
#' }
#'
#' @export
eq_map <- function(data, annot_col) {
  # note that annot_col in a character string
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = data, radius = ~ Mag,
                              lng = ~ Longitude, lat = ~ Latitude,
                              popup = ~ data[[annot_col]])
}



#' @title Leaflet Earth Quake Map with Customized Label
#'
#' @description \code{eq_create_label} creates a more informative popup using HTML tags
#'     to be used as labels in \code{\link{eq_map}}.
#'
#' @details This function put together a character string for each earthquake to be
#' used as a more informative label. The label contains the following items:
#' \itemize{
#'  \item Location
#'  \item Magnitude
#'  \item Total Deaths
#'  }
#'
#' @param data The \code{\link{NOAA_data}} data frame.
#'
#' @return A character vector using HTML tags to be used as label for
#'  \code{annot_col} variable.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' NOAA_data <- NOAA_data %>%
#' eq_clean_data() %>%
#' eq_location_clean() %>%
#'  dplyr::filter(Country %in% c("HONDURAS", "MEXICO") & lubridate::year(Date) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.))
#'
#'  eq_map(data = NOAA_data, annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data) {
  labelled_data <- data %>%
    dplyr::mutate(popup_text = paste("<b>Location:</b>", Location, "<br />",
                                     "<b>Magnitude:</b>", Mag, "<br />",
                                     "<b>Total Deaths:</b>", Deaths, "<br />"),
                  popup_text = ifelse(is.na(Location) | is.na(Mag) | is.na(Deaths),
                                      paste("<b>No Data Available</b>"), popup_text))
  labelled_data$popup_text
}