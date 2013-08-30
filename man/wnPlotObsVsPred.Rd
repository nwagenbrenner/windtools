\name{wnPlotObsVsPred}
\alias{wnPlotObsVsPred}
\title{Plot observed vs predicted values}
\usage{
  wnPlotObsVsPred(df, var)
}
\arguments{
  \item{df}{dataframe returned from wnBuildBiasDf()}

  \item{var}{variable to plot ('speed' or 'direction')}
}
\value{
  ggplot2 object
}
\description{
  \code{wnPlotObsVsPred} plots observed vs. predicted data
}
\details{
  Returns a scatter plot of observed vs predicted speed or
  direction data.
}

