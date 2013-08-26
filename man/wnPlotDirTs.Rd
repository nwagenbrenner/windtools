\name{wnPlotDirTs}
\alias{wnPlotDirTs}
\title{Make a faceted wind direction time series plot}
\usage{
  wnPlotDirTs(df)
}
\arguments{
  \item{df}{dataframe returned from wnBuildTsDf()}
}
\value{
  ggplot2 object
}
\description{
  \code{wnPlotDirTs} plots wind direction time series
  faceted on sensor and weather model type
}
\details{
  Plots the wind direction time series faceted on weather
  model type if the supplied dataframe contains a single
  sensor. If the dataframe contains multiple sensors, the
  time series plot is facect on sensor and weather model
  type.
}

