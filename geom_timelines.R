#' @title Geom for creating a timeline from earthquake data
#'
#' @description This function creates a new geom to create a timeline for a specified
#'     data range and plots each earthquake as a point on that timline
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
#' @examples
#' \dontrun{
#' ggplot(data = eq, aes(x = Date, y = Country,
#' color = Deaths, size = Mag)) + geom_timeline(alpha = 0.2)
#' }
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", show.legend = NA,
                          na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#' @title ggproto timeline object of class ggproto
#'
#' @description This is the setup required for creating a new geom class. This GeomTimeLine
#'     inherits from a top level class called Geom.It creates a geom that draws the
#'     timeline for a specified date interval and put points on it for each earthquake.
#'
#' @export
GeomTimeLine <- ggplot2::ggproto("GeomTimeLine", ggplot2::Geom,
                                 required_aes = c("x"),
                                 non_missing_aes = c("size", "shape", "colour", "y"),
                                 default_aes = ggplot2::aes(y = 0.5, shape = 19, colour = "grey",
                                                            size = 5, fill = "grey20", alpha = 0.2,
                                                            stroke = 0.5),
                                 draw_key = ggplot2::draw_key_point,
                                 
                                 draw_panel = function(data, panel_params, coord) {
                                   coords <- coord$transform(data, panel_params)
                                   
                                   points <- grid::pointsGrob(
                                     coords$x, coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(
                                       col = alpha(coords$colour, coords$alpha),
                                       fill = alpha(coords$fill, coords$alpha),
                                       lwd = coords$stroke * .pt,
                                       fontsize = coords$size * .pt
                                     )
                                   )
                                   
                                   lines <- purrr::map(unique(coords$y), function(x)
                                     grid::linesGrob(y = c(x, x)))
                                   params <- c(list(points), lines)
                                   # Since gList does not support dynamic dots, I spliced
                                   # the params with !!!
                                   grid::gTree(children = rlang::exec(grid::gList, !!!params))
                                 }
)

