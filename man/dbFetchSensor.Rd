\name{dbFetchSensor}
\alias{dbFetchSensor}
\title{Fetch all data from a single sensor for a specified time period}
\usage{
  dbFetchSensor(db, sensor, start_time, end_time)
}
\arguments{
  \item{db}{wind database to query}

  \item{sensor}{sensor to extract info for}

  \item{start_time}{format is '2011-08-15 06:00:00'}

  \item{end_time}{format is '2011-08-15 06:00:00'}
}
\value{
  dataframe with id, date/time, speed, gust, direction, and
  quality
}
\description{
  \code{dbFetchSensor} returns a dataframe from an SQLite
  database for a single senosr for a specified time period
}
\details{
  This fucntion returns a dataframe of raw, 30-s wind data
  for a single sensor for a specified time period.
}

