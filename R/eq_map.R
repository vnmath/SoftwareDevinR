#' Module 1: This maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in
#' pop up window containing annotation data stored in a column of the data frame
#'
#' @param mapdata A data frame with data obtained from NOAA website
#' @param annot_col The name of the column from the data to be use for annotation
#'
#' @return A map of the earthquakes epicenters and giving some annotations
#'
#' @details The cleaning of the data frame is done with the eq_clean_data() function of this package.
#' The function return a map of the earthquakes epicenters (LATITUDE/LONGITUDE) and annotates each point with
#' in pop up window containing annotation data stored in a column of the cleaned data frame.
#' On the map, each earthquake is shown with a circle, and the radius of the circle
#' is proportional to the earthquake's magnitude (EQ_PRIMARY).
#'
#' @examples
#' \dontrun{
#' readr::read_delim("signif.txt", delim = "\t") %>%
#' eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @export

eq_map <- function(mapdata, annot_col = "DATE") {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = mapdata$LONGITUDE, lat = mapdata$LATITUDE,
                              radius = as.numeric(mapdata$EQ_PRIMARY), popup = mapdata[[annot_col]],
                              stroke = FALSE, fillOpacity = 0.5)
}

#' Create a function called eq_create_label() that takes the dataset
#' as an argument and creates an HTML label in the leaflet map.
#' @param mapdata A cleaned data frame from NOAA website
#'
#' @return An HTML label in the leaflet map.
#'
#' @details This function should put together a character string for
#' each earthquake that will show the cleaned location
#'
#' @examples
#' \dontrun{
#' readr::read_delim("signif.txt", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   eq_location_clean() %>%
#'   dplyr::filter(COUNTRY == "CANADA" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
#'
#' @export

eq_create_label <- function(mapdata){
  paste(ifelse(is.na(mapdata$LOCATION_NAME),"", paste("<b>Location: </b>",mapdata$LOCATION_NAME,"<br/>")),
        ifelse(is.na(mapdata$EQ_PRIMARY),"", paste("<b>Magnitude: </b>",mapdata$EQ_PRIMARY,"<br/>")),
        ifelse(is.na(mapdata$TOTAL_DEATHS),"", paste("<b>Total deaths: </b>",mapdata$TOTAL_DEATHS,"<br/>")))
}
