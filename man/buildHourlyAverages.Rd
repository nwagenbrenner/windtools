\name{buildHourlyAverages}
\alias{buildHourlyAverages}
\title{Build a dataframe with hourly averaged data}
\usage{
buildHourlyAverages(df)
}
\arguments{
  \item{df}{dataframe}
}
\value{
dataframe with hourly averages
}
\description{
\code{buildHourlyAverages} returns dataframe with hourly
averaged data
}
\details{
This fucntion returns a dataframe with hourly averages of
wind data. Data are averaged over all timesteps to produce
hourly averages for each hour of the day. This is useful to
see, for example, what the typcial wind field looks like at
1000 LT. It may be most usefule to call this function after
other subsetting operations have been done. For example,
you may want to first subset on speed to filter out
high-wind event cases to examine diurnal wind patterns.
}
\examples{
data(wind)
s <- subsetOnSpeed(wind, 'R2', '<', 6.0)
s.avg <- buildHourlyAverages(s)
}

