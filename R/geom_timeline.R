#' Module 2: Visualize the times at which earthquakes occur within certain countries
#'
#' @return Produce a time line plot of selected earthquakes.
#'
#' @details The data to be used with this geom from NOAA website
#'
#' @inheritParams ggplot2::layer
#' @param mapping Create asthetics mapping.
#' @param data  The data
#' @param stat  The statistical transformation as a string.
#' @param position Position adjustment
#' @param show.legend show the legends or not
#' @param inherit.aes the default aesthetics
#'
#' @param na.rm  Missing value
#' @param ... Other parameters
#'
#'#' @section Aesthetics:
#' \code{geom_timeline} has the following aesthetics
#'
#' \itemize{
#'   \item \strong{\code{x}} # Time variable
#'   \item \code{y}          # Factor indicating some stratification
#'   \item \code{color}      # Color of border of elements
#'   \item \code{shape}      # Shape
#'   \item \code{size}       # Size
#'   \item \code{alpha}      # Transparency (1: opaque; 0: transparent)
#'   \item \code{fill}       # Color of inside of elements
#'   \item \code{stroke}     # Stroke
#' }
#'
#' @importFrom ggplot2 layer
#'
#' @export
#'
#' @examples
#' # The data must be cleaned using the function \code{eq_clean_data}
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' data <- eq_clean_data(data)
#' data %>%
#' dplyr::filter(COUNTRY == c("MEXICO","USA") & lubridate::year(DATE) >= 2010) %>%
#' ggplot(aes(x=DATE,y=COUNTRY,color=TOTAL_DEATHS,size=EQ_PRIMARY)) +
#' geom_timeline(alpha=.5) +
#' theme(legend.position="bottom", legend.box="horizontal", plot.title=element_text(hjust=0.5)) +
#' ggtitle("Earthquakes Visualization Tool") +
#' labs(size = "Richter scale value", color = "# deaths")
#' }
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' GeomTimeline Geom
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid segmentsGrob gpar pointsGrob gList
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = "x",
                                 default_aes = ggplot2::aes(y=0, colour="black", shape=19, size=1, stroke = 0.5, alpha = 0.5, fill = NA),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_params, coord) {
                                   coords <- coord$transform(data, panel_params)
                                   grid::gList(
                                     grid::pointsGrob(
                                       coords$x, coords$y,
                                       pch = coords$shape,
                                       gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
                                                       fill = alpha(coords$fill, coords$alpha),
                                                       fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                       lwd = coords$stroke * .stroke / 2)
                                     ),
                                     grid::segmentsGrob(
                                       x0 = min(coords$x), y0 = coords$y, x1 = max(coords$x), y1 = coords$y,
                                       gp = grid::gpar(col = "black", lwd = 1)
                                     )
                                   )
                                 }
)
