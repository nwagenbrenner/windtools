\name{dbFetchAll}
\alias{dbFetchAll}
\title{Fetch all data from a database for specified time period}
\usage{
  dbFetchAll(drv, db, start_time, end_time)
}
\arguments{
  \item{drv}{database driver ("SQLite" is only option right
  now)}

  \item{db}{database to query}

  \item{start_time}{format is '2011-08-15 06:00:00'}

  \item{end_time}{format is '2011-08-15 06:00:00'}
}
\value{
  dataframe with id, date/time, speed, gust, direction, and
  quality
}
\description{
  \code{dbFetchAll} returns a dataframe of wind data for
  specified time period
}
\details{
  This fucntion returns a dataframe of raw, 30-s wind data
  for a specified time period. Currently only connects to
  SQLite db.
}

