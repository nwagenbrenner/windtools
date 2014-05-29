\name{wnBuildTsDf}
\alias{wnBuildTsDf}
\title{Build a subdataset from main bias dataframe for time series plots}
\usage{
wnBuildTsDf(df, sensorList = "", notSensorList = "")
}
\arguments{
  \item{df}{main bias dataframe}

  \item{sensorList}{list of sensors to include}

  \item{notSensorList}{list of sensors to omit}
}
\value{
dataframe for use with wn* plotting functions
}
\description{
\code{wnBuildTsDf} subsets main bias dataframe for time
series plots
}
\details{
Builds a subdataset from main bias dataframe that can be
used for time series plots. Either sensorList or
notSensorList should be supplied. If neither sensorList or
nonSensorList is supplied, all sensors will be included.
}

