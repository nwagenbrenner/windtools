\name{wnBoxplot}
\alias{wnBoxplot}
\title{Make a box plot}
\usage{
wnBoxplot(df, var, jitter = FALSE)
}
\arguments{
  \item{df}{dataframe returned from wnBuildBiasDf()}

  \item{var}{variable to plot ('bias_speed' or 'bias_dir')}

  \item{jitter}{indicates if points should be plotted
  (TRUE, FALSE)}
}
\value{
ggplot2 object
}
\description{
\code{wnBoxplot} makes box plots for bias_speed or bias_dir
in main bias dataframe
}
\details{
Makes box plots for either speed or direction.
}

