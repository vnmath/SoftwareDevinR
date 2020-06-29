## ----eval = FALSE-------------------------------------------------------------
#  library(readr)
#  data <- readr::read_delim("signif.txt", delim = "\t")

## ----eval = FALSE-------------------------------------------------------------
#  clean_data <- eq_clean_data(data)
#  clean_data <- eq_location_clean(clean_data)

## ----eval = FALSE-------------------------------------------------------------
#  clean_data %>%
#    dplyr::filter(COUNTRY == "CANADA" & lubridate::year(DATE) >= 2000) %>%
#    eq_map(annot_col = "DATE")

## ----eval = FALSE-------------------------------------------------------------
#  clean_data %>%
#    dplyr::filter(COUNTRY == "CANADA" & lubridate::year(DATE) >= 2000) %>%
#    dplyr::mutate(popup_text = eq_create_label(.)) %>%
#    eq_map(annot_col = "popup_text")

## ----eval = FALSE-------------------------------------------------------------
#  data %>%
#    dplyr::filter(COUNTRY == c("CANADA","USA") & lubridate::year(DATE) >= 2010) %>%
#    ggplot(aes(x=DATE,y=COUNTRY,color=TOTAL_DEATHS,size=EQ_PRIMARY)) +
#    geom_timeline(alpha=.5) +
#    theme(legend.position="bottom", legend.box="horizontal", plot.title=element_text(hjust=0.5)) +
#    ggtitle("Earthquakes")

## ----eval = FALSE-------------------------------------------------------------
#  data %>%
#    dplyr::filter(COUNTRY == c("CANADA","USA") & lubridate::year(DATE) >= 2010) %>%
#    ggplot(aes(x=DATE,y=COUNTRY,color=TOTAL_DEATHS,size=EQ_PRIMARY)) +
#    geom_timeline(alpha=.5) +
#    geom_timelinelabel(aes(label=LOCATION_NAME),n_max=3) +
#    theme(legend.position="bottom", legend.box="horizontal", plot.title=element_text(hjust=0.5)) +
#    ggtitle("Earthquakes")

