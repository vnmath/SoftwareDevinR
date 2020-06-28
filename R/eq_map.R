#' This maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in
#' pop up window containing annotation data stored in a column of the data frame
#'
#' @param mapdata A data frame with data obtained from NOAA website
#' @param annot_col The name of the column from the data to be use for annotation
#'
#' @return A map of the earthquakes epicenters and giving some annotations
#'
#' @details After downloading, reading and cleaning the dataset from NOAA site,
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}.
#' National Geophysical Data Center / World Data Service (NGDC/WDS): Significant Earthquake Database.
#' National Geophysical Data Center, NOAA.
#' The cleaning of the data frame is done with the eq_clean_data() function of this package.
#' The function return a map of the earthquakes epicenters (LATITUDE/LONGITUDE) and annotates each point with
#' in pop up window containing annotation data stored in a column of the cleaned data frame.
#' The user is able to choose which column is used for the annotation in the pop-up with this function
#' by using the argument named "annot_col". If the "annot_col" argument is not used, then the value of the "DATE" column is used.
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
#' as an argument and creates an HTML label that can be used as
#' the annotation text in the leaflet map.
#' @param mapdata A cleaned data frame with data obtained from NOAA website
#'
#' @return An HTML label that can be used as the annotation text in the leaflet map.
#'
#' @details This function should put together a character string for
#' each earthquake that will show the cleaned location (as cleaned by
#' the eq_location_clean() function created in Module 1),
#' the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS),
#'  with boldface labels for each ("Location", "Total deaths", and "Magnitude").
#'  If an earthquake is missing values for any of these,
#' both the label and the value should be skipped for that element of the tag.
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
