\name{wnPlotBiasVsObs}
\alias{wnPlotBiasVsObs}
\title{Plot bias vs observed data}
\usage{
  wnPlotBiasVsObs(df, var)
}
\arguments{
  \item{df}{dataframe returned from wnBuildBiasDf()}

  \item{var}{variable to plot ('speed' or 'direction')}
}
\value{
  ggplot2 object
}
\description{
  \code{wnPlotBiasVsObs} plots bias vs observed data
  faceted on wx model type
}
\details{
  Returns a scatter plot of speed or direction bias vs
  observed data faceted on weather model type.
}

