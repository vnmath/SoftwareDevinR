utils::globalVariables(c("LOCATION_NAME","I_D","YEAR","MONTH","DAY","LATITUDE","LONGITUDE",
                           "EQ_PRIMARY","COUNTRY","STATE","TOTAL_DEATHS","DATE","YEAR4"))
#' Module 1: Cleaning NOAA earthquake data
#'
#' @param datatoclean A data frame with raw data obtained from NOAA website
#'
#' @return A data frame with cleaned date, latitude and longitude numerical columns
#'
#' @details The function returns a date column created by uniting the year, month, day and
#' converting it to the Date class
#' @examples
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' data <- eq_clean_data(data)
#' }
#'
#' @importFrom dplyr %>% mutate select if_else
#' @importFrom tidyr unite
#' @importFrom lubridate year ymd
#'
#' @export
eq_clean_data <- function(datatoclean) {
  clean_data <- datatoclean %>%
    dplyr::select(I_D, YEAR, MONTH, DAY, LATITUDE, LONGITUDE, LOCATION_NAME, EQ_PRIMARY, COUNTRY, STATE, TOTAL_DEATHS) %>%
    dplyr::mutate(YEAR4=sprintf("%04d",as.numeric(gsub('-','',YEAR)))) %>%
    dplyr::mutate(MONTH=dplyr::if_else(is.na(MONTH),'01',sprintf("%02d", MONTH))) %>%
    dplyr::mutate(DAY=dplyr::if_else(is.na(DAY),'01',sprintf("%02d", DAY))) %>%
    tidyr::unite(DATE,YEAR4,MONTH,DAY,sep='-',remove = FALSE) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    dplyr::select(-YEAR4)

  lubridate::year(clean_data$DATE) <- clean_data$YEAR

  clean_data <- clean_data %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE),LONGITUDE = as.numeric(LONGITUDE),
                  EQ_PRIMARY = as.numeric(EQ_PRIMARY), TOTAL_DEATHS = as.numeric(TOTAL_DEATHS))
  clean_data
}

#' Cleaning NOAA earthquake data location values
#'
#' @param locationtoclean A data frame with raw data obtained from NOAA website
#'
#' @return A data frame with cleaned LOCATION_NAME column
#'
#' @details The function cleans the LOCATION_NAME column
#'
#' @examples
#' \dontrun{
#' library(readr)
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' data <- eq_location_clean(data)
#' }
#'
#' @importFrom dplyr %>% mutate
#'
#' @export

eq_location_clean <- function(locationtoclean) {
  location_clean <- locationtoclean %>%
    dplyr::mutate(LOCATION_NAME=gsub("^.*:"," ",LOCATION_NAME)) %>%
    dplyr::mutate(LOCATION_NAME=gsub("\\b([[:alpha:]])([[:alpha:]]+)", "\\U\\1\\L\\2" ,LOCATION_NAME, perl=TRUE))
  location_clean
}
