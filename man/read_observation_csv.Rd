% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csv_wrangling.R
\name{read_observation_csv}
\alias{read_observation_csv}
\title{read_observation_csv}
\usage{
read_observation_csv(filepath, convert_coordinates = T)
}
\arguments{
\item{filepath}{Location of the file.}

\item{convert_coordinates}{Convert coordinates (if you are using them instead of named area) from northings/eastings to latitude/longitude.}
}
\value{
A dataframe with the processed data (and pertinent metadata) in tall format.
}
\description{
Read a UKCP observation data .csv file and handle some basic processing.
}
\examples{
\dontrun{
read_observation_csv(
  "filename.csv"
)
}

}
