% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_clean_data.R
\name{eq_clean_data}
\alias{eq_clean_data}
\title{Module1: Cleaning NOAA earthquake data}
\usage{
eq_clean_data(datatoclean)
}
\arguments{
\item{datatoclean}{A data frame with raw data obtained from NOAA website}
}
\value{
A data frame with cleaned date, latitude and longitude numerical columns
}
\description{
Module1: Cleaning NOAA earthquake data
}
\details{
The function returns a date column created by uniting the year, month, day and
converting it to the Date class
}
\examples{
\dontrun{
data <- readr::read_delim("signif.txt", delim = "\t")
data <- eq_clean_data(data)
}

}
