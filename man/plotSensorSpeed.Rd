\name{plotSensorSpeed}
\alias{plotSensorSpeed}
\title{Plot wind speed as a time series}
\usage{
  plotSensorSpeed(df, sensor, threshold = NULL)
}
\arguments{
  \item{df}{dataframe}

  \item{sensor}{name of sensor ('plot')}

  \item{threshold}{threshold speed to indicate with
  horizontal bar}
}
\value{
  ggplot2 object
}
\description{
  \code{plotSensorSpeed} returns speed time series as a
  ggplot2 object
}
\details{
  This fucntion creates a ggplot2 object of wind speed vs.
  time for a single sensor.
}
\examples{
data(wind)
plotSensorSpeed(wind, 'R26')
}

