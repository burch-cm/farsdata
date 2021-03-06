% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Read FARS data from given years}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{a character or numeric vector of target years from which to load data}
}
\value{
a list of data frames containing data from each target year
}
\description{
Reads in FARS data files from a given vector of years.
}
\details{
Loads a list of data frames of FARS data from the specified years.
}
\examples{
\dontrun{
fars_read_years(2017)
fars_read_years(c("2016", "2017"))
fars_read_years(2010:2017)
}
}

