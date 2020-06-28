#' Module 2: Visualization Tools: Build a geom for ggplot2 called geom_timeline()
#' for plotting a time line of earthquakes
#' ranging from xmin to xmaxdates with a point for each earthquake.
#'

#'
#' @return A time line plot of certain earthquakes.
#'
#' @details The data is downloaded and readed from NOAA website,
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}.
#'
#' @inheritParams ggplot2::layer
#' @param mapping The mappings created.
#' @param data  The data need to display in this layer.
#' @param stat  The statistical transformation to use on the data for this layer.
#' @param position Position adjustment
#' @param show.legend logical. This layer be included in the legends or not
#' @param inherit.aes Override the default aesthetic if false
#' @param na.rm Missing values are removed with if false
#' @param ... Other arguments passed on to [layer()].
#'
#' \code{geom_timeline} understands the following aesthetics
#'
#' \itemize{
#'   \item \code{x}          # Time variable (required)
#'   \item \code{y}          # Stratification
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
#' # The data need to be cleaned using the function eq_clean_data
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' data <- eq_clean_data(data)
#' data %>%
#' dplyr::filter(COUNTRY == c("CANADA","USA") & lubridate::year(DATE) >= 2010) %>%
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

#' GeomTimeline
#'
#' GeomTimeline Geom code
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
