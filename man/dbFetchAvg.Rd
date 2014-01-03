\name{dbFetchAvg}
\alias{dbFetchAvg}
\title{Fetch averaged data from a database for specified time period}
\usage{
  dbFetchAvg(drv, db, start_time, end_time, avg_time)
}
\arguments{
  \item{drv}{database driver ("SQLite" is only option right
  now)}

  \item{db}{database to query}

  \item{start_time}{format is '2011-08-15 06:00:00'}

  \item{end_time}{format is '2011-08-15 06:00:00'}

  \item{avg_time}{averaging time in minutes}
}
\value{
  dataframe with id, date/time, speed, gust, direction, and
  quality
}
\description{
  \code{dbFetchAvg} returns a dataframe of wind data for
  specified time period
}
\details{
  This fucntion returns a dataframe of averaged data for a
  specified time period. The data is averaged at the top of
  the hour. Currently only connects to SQLite db.
}

