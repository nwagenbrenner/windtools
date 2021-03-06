% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{dbFetchHourlyAvg}
\alias{dbFetchHourlyAvg}
\title{Fetch averaged data from an SQLite database for a specified time period}
\usage{
dbFetchHourlyAvg(db, sensor, start_time, end_time, avg_time, align = "center")
}
\arguments{
\item{db}{database to query}

\item{sensor}{sensor to extract info for}

\item{start_time}{format is '2011-08-15 06:00:00'}

\item{end_time}{format is '2011-08-15 06:00:00'}

\item{avg_time}{averaging time in minutes}

\item{align}{where to align the index of the result (left, middle, right)}
}
\value{
time series of wind speed in xts format
}
\description{
\code{dbFetchHourlyAvg} returns hourly averages from an SQLite database for a specified time period
}
\details{
This function returns a time series of hourly averaged wind speed data for a specified
time period. The data is averaged at the top of the hour and aligned
according to the 'align' parameter. Currently only connects to SQLite db.
}
