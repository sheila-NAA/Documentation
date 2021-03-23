#' @title Label the Biggest Earthquakes on the Richter Scales
#'
#' @description \code{geom_timeline_label} enables you to label the n biggest earthquakes
#'     based on the Richter scales by means of \code{n_max} arguments.
#'
#' @param mapping Set of aesthetis mappings created by \code{\link[ggplot2]{aes}}.
#' @param data The data to be displayed in this layer.
#' @param position Position adjustment, either as a string, or the result of a
#'  call to position adjustment function.
#' @param ... Other arguments passed on to \code{link[ggplot2]{layer}}.
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#'  If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA,
#'  the default, includes if any aesthetics are mapped. FALSE never includes,
#'  and TRUE always includes. It can also be a named logical vector to finely
#'  select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#'  combining with them.
#' @param stat Override the default connection between \code{geom_timeline_label}
#' and stat_timeline_label(however I have not written a stat for this geom).
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' NOAA_data <- NOAA_data %>%
#' eq_clean_data() %>%
#' eq_location_clean() %>%
#' dplyr::filter(Country %in% c("MEXICO", "IRAN") &
#'                Date %within% lubridate::interval(ymd(20000103), ymd(20180104))) %>%
#' dplyr::mutate(Country = factor(Country, levels = unique(Country))) %>%
#' ggplot2::ggplot() +
#' geom_timeline(ggplot2::aes(x = Date, y = Country, size = Mag, colour = Deaths)) +
#' geom_timeline_label(ggplot2::aes(x = Date, y = Country, label = Location,
#'                          magnitude = Mag, colour = Deaths, n_max = 7), alpha = 0.5) +
#' ggplot2::scale_colour_continuous(name = "# deaths") +
#' ggplot2::theme(legend.position = "bottom") +
#' ggplot2::ylab("")
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", show.legend = NA,
                                na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeLineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @title GeomTimelineLabel
#'
#' @description This is the setup required for creating a new geom class.
#'     This GeomTimeLineLabel inherits from a top level class called Geom.
#'     It creates a geom that draws the timeline for a specified date interval
#'     and put points on it for each earthquake. It then creates labels in
#'     order to annotate the biggest earthquakes on the timeline.
#'
#' @export
GeomTimeLineLabel <- ggplot2::ggproto("GeomTimeLineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label", "magnitude"),
                                      non_missing_aes = c("size", "shape", "colour", "y"),
                                      default_aes = ggplot2::aes(y = 0.5, shape = 19,
                                                                 colour = "grey",
                                                                 size = 3, fill = "grey20", alpha = 0.5,
                                                                 stroke = 0, n_max = 5),
                                      draw_key = ggplot2::draw_key_point,
                                      
                                      draw_panel = function(data, panel_scales, coord) {
                                        
                                        n_max <- data$n_max[1]
                                        
                                        data <- data %>%
                                          dplyr::group_by(group) %>%
                                          dplyr::slice_max(magnitude, n = n_max)
                                        
                                        coords <- coord$transform(data, panel_scales)
                                        
                                        points <- grid::pointsGrob(
                                          coords$x, coords$y,
                                          pch = coords$shape,
                                          gp = grid::gpar(
                                            col = alpha(coords$colour, coords$alpha),
                                            fill = alpha(coords$fill, coords$alpha),
                                            lwd = coords$size * .pt,
                                            fontsize = coords$size * .pt
                                          )
                                        )
                                        
                                        labels <- grid::textGrob(
                                          coords$label,
                                          coords$x, coords$y + 0.15,
                                          coords$just <- "centre",
                                          coords$hjust <- "right",
                                          coords$vjust <- "top",
                                          rot = 45,
                                          gp = grid::gpar(lwd = coords$size * .pt)
                                        )
                                        
                                        seg_label <- grid::segmentsGrob(
                                          x0 = coords$x,
                                          y0 = coords$y,
                                          x1 = coords$x,
                                          y1 = coords$y + 0.1
                                        )
                                        
                                        lines <- purrr::map(unique(coords$y), function(x)
                                          grid::linesGrob(y = c(x, x)))
                                        params <- c(list(points), lines, list(labels),
                                                    list(seg_label))
                                        # Since gList doesn't support dynamic dots I spliced
                                        # params with !!!
                                        grid::gTree(children = rlang::exec(grid::gList,  !!!params))
                                      }
)
