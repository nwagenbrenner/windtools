\name{wnPlotObsVsPred}
\alias{wnPlotObsVsPred}
\title{Plot observed vs predicted values}
\usage{
wnPlotObsVsPred(df, var, color_list = NULL)
}
\arguments{
  \item{df}{dataframe returned from wnBuildBiasDf()}

  \item{var}{variable to plot ('speed' or 'direction')}

  \item{color_list}{optional list of colors to use for
  legend}
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

