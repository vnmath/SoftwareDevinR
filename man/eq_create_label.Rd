% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_map.R
\name{eq_create_label}
\alias{eq_create_label}
\title{Create a function called eq_create_label() that takes the dataset
as an argument and creates an HTML label that can be used as
the annotation text in the leaflet map.}
\usage{
eq_create_label(mapdata)
}
\arguments{
\item{mapdata}{A cleaned data frame with data obtained from NOAA website}
}
\value{
An HTML label that can be used as the annotation text in the leaflet map.
}
\description{
Create a function called eq_create_label() that takes the dataset
as an argument and creates an HTML label that can be used as
the annotation text in the leaflet map.
}
\details{
This function should put together a character string for
each earthquake that will show the cleaned location (as cleaned by
the eq_location_clean() function created in Module 1),
the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS),
 with boldface labels for each ("Location", "Total deaths", and "Magnitude").
 If an earthquake is missing values for any of these,
both the label and the value should be skipped for that element of the tag.
}
\examples{
\dontrun{
readr::read_delim("signif.txt", delim = "\t") \%>\%
  eq_clean_data() \%>\%
  eq_location_clean() \%>\%
  dplyr::filter(COUNTRY == "CANADA" & lubridate::year(DATE) >= 2000) \%>\%
  dplyr::mutate(popup_text = eq_create_label(.)) \%>\%
  eq_map(annot_col = "popup_text")
}

}
