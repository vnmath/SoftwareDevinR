filename <- system.file("inst/data", "signif.txt", package="SoftwareDevinR")
data <- readr::read_delim(filename, delim = '\t')
clean_data <- eq_clean_data(data)

#' Test eq_clean_data function
testthat::expect_that(clean_data$DATE, testthat::is_a('Date'))
testthat::expect_that(clean_data$LATITUDE, testthat::is_a('numeric'))
testthat::expect_that(clean_data$LONGITUDE, testthat::is_a('numeric'))
testthat::expect_that(clean_data$TOTAL_DEATHS, testthat::is_a('numeric'))


#' Test eq_map function
testthat::expect_that(clean_data %>%
                        dplyr::filter(COUNTRY == 'USA' & lubridate::year(DATE) >= 2000) %>%
                        eq_map(annot_col = 'DATE'), testthat::is_a('leaflet'))


#' Test eq_create_label function
testthat::expect_that(eq_create_label(clean_data), testthat::is_a('character'))

testthat::expect_that(clean_data %>%
                        dplyr::filter(COUNTRY == 'CANADA' & lubridate::year(DATE) >= 2000) %>%
                        dplyr::mutate(popup_text = eq_create_label(.)) %>%
                        eq_map(annot_col = 'popup_text'), testthat::is_a('leaflet'))

#' Test eq_location_clean function
clean_data <- eq_location_clean(clean_data)
testthat::expect_that(clean_data,testthat::is_a('data.frame'))
