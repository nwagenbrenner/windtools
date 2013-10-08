\name{wnPlotSpeedTs}
\alias{wnPlotSpeedTs}
\title{Make a faceted wind speed time series plot}
\usage{
  wnPlotSpeedTs(df, color_list = NULL, xscale = NULL)
}
\arguments{
  \item{df}{dataframe returned from wnBuildTimeSeriesDf()}

  \item{color_list}{optional list of colors to use (e.g.,
  colors <- c("WindNinja" = "red", "NWP" = "darkblue"))}

  \item{xscale}{optional scale_x_datetime() argument}
}
\value{
  ggplot2 object
}
\description{
  \code{wnPlotSpeedTs} plots wind speed time series faceted
  on sensor and weather model type
}
\details{
  Plots the wind speed time series faceted on NWP model and
  model type
}

