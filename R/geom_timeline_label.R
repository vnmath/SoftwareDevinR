#' Module 2: Visualization Tools: Visualize the times at which earthquakes occur within certain countries
#'This geom adds a vertical line to each data point with a text annotation
#'(e.g. the location of the earthquake) attached to each line. There should be an option
#'to subset to n_max number of earthquakes, where we take the n_max largest (by magnitude) earthquakes.
#'Aesthetics are x, which is the date of the earthquake
#'and label which takes the column name from which annotations will be obtained.
#' @return The geom add annotations to the n_max largest magnitude earthquakes.
#'
#' @details The data downloaded from \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}.
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
#' \code{geom_timeline_label} understands the following aesthetics
#' \itemize{
#'   \item \strong{\code{x}}     # Time variable
#'   \item \strong{\code{label}} # Annotations to the earthquake data
#'   \item \code{y}              # Stratification
#'   \item \code{n_max}          # number of earthquakes of subset corresponding to their magnitude (EQ_PRIMARY)
#'   \item \code{y_length}       # vertical line length to each data point
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
#' data <- eq_location_clean(data)
#' data %>%
#' dplyr::filter(COUNTRY == c("CANADA","USA") & lubridate::year(DATE) >= 2010) %>%
#' ggplot(aes(x=DATE,y=COUNTRY,color=TOTAL_DEATHS,size=EQ_PRIMARY)) +
#' geom_timeline(alpha=.5) +
#' geom_timelinelabel(aes(label=LOCATION_NAME),n_max=5) +
#' theme(legend.position="bottom", legend.box="horizontal", plot.title=element_text(hjust=0.5)) +
#' ggtitle("Earthquakes Visualization") +
#' labs(size = "Richter scale", color = "# deaths")
#' }
geom_timelinelabel <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelinelabel, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' GeomTimelinelabel
#'
#' GeomTimelinelabel Geom coding
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid segmentsGrob gpar textGrob gList
#' @importFrom dplyr slice arrange_ group_by_ %>%
#'
#' @export
GeomTimelinelabel <- ggplot2::ggproto("GeomTimelinelabel", ggplot2::Geom,
                                      required_aes = c("x","label"),
                                      default_aes = ggplot2::aes(y=0, n_max=0, y_length=1),
                                      draw_key = ggplot2::draw_key_point,
                                      draw_panel = function(data, panel_params, coord) {

                                        if (data$n_max[1]>0){
                                          if (data$y[1]==0){
                                            data<- data %>%
                                              dplyr::arrange_(~ desc(size)) %>%
                                              dplyr::slice(1:data$n_max[1])
                                          }
                                          else {
                                            data<- data %>%
                                              dplyr::arrange_(~ desc(size)) %>%
                                              dplyr::group_by_(~ y) %>%
                                              dplyr::slice(1:data$n_max[1])
                                          }
                                        }
                                        if (!data$y[1]==0){
                                          data$y_length<-dim(table(data$y))
                                        }

                                        coords <- coord$transform(data, panel_params)
                                        grid::gList(
                                          grid::segmentsGrob(
                                            x0 = coords$x, y0 = coords$y, x1 = coords$x, y1 = (.2/coords$y_length)+coords$y,
                                            gp = grid::gpar(col = "black", lwd = .5)
                                          ),
                                          grid::textGrob(
                                            label = coords$label,
                                            x = coords$x, y = (.2/coords$y_length)+coords$y , just = "left", rot = 45
                                          )
                                        )
                                      }
)
