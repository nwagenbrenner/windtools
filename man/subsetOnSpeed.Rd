\name{subsetOnSpeed}
\alias{subsetOnSpeed}
\title{Subset wind data on speed criteria}
\usage{
  subsetOnSpeed(df, sensor, condition, threshold)
}
\arguments{
  \item{df}{dataframe}

  \item{sensor}{name of sensor ('plot')}

  \item{condition}{'<' or '>'}

  \item{threshold}{threshold speed to subset on}
}
\value{
  subsetted dataframe
}
\description{
  \code{subsetOnSpeed} returns subsetted dataframe
}
\details{
  This fucntion subsets the wind data frame based on speed
  criteria for a single sensor.
}
\examples{
data(wind)
s <- subsetOnSpeed(wind, 'R2', '<', 6.0)
}

