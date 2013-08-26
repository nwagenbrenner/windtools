\name{wnPlotSpeedTs}
\alias{wnPlotSpeedTs}
\title{Make a faceted wind speed time series plot}
\usage{
  wnPlotSpeedTs(df)
}
\arguments{
  \item{df}{dataframe returned from wnBuildTimeSeriesDf()}
}
\value{
  ggplot2 object
}
\description{
  \code{wnPlotSpeedTs} plots wind speed time series faceted
  on sensor and weather model type
}
\details{
  Plots the wind speed time series faceted on weather model
  type if the supplied dataframe contains a single sensor.
  If the dataframe contains multiple sensors, the time
  series plot is facect on sensor and weather model type.
}

