% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wn_plot_bias.R
\name{wnCreateBiasVectorMap}
\alias{wnCreateBiasVectorMap}
\title{Create vector maps of overlaid observed and predicted winds}
\usage{
wnCreateBiasVectorMap(df, colors = FALSE)
}
\arguments{
\item{df}{dataframe returned from buildBiasHourlyAverages}

\item{colors}{color vectors by speed (TRUE/FALSE)}
}
\value{
plotGoogleMaps object
}
\description{
\code{wnCreateBiasVectorMap} creates bubble maps of wind prediction errors
}
\details{
Returns a vector map of observed and predcited winds.
}
