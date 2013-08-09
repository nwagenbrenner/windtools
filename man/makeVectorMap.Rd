\name{makeVectorMap}
\alias{makeVectorMap}
\title{Make a vector map of the wind field}
\usage{
  makeVectorMap(df, lat, lon, zoom, maptype)
}
\arguments{
  \item{df}{dataframe}

  \item{lat}{center lat of Google Maps image}

  \item{lon}{center lon of Google Maps image}

  \item{zoom}{zoom for Google Maps image (1-20)}

  \item{maptype}{type of Google Maps image (terrain,
  hybrid, satellite, roadmap)}
}
\value{
  ggmap object representation of the wind field
}
\description{
  \code{makeVectorMap} returns a ggmap object of the vector
  field
}
\details{
  This fucntion returns a vector plot of the wind field
  overlayed on a static Google Maps image. If multiple
  hours are supplied, the plot is faceted on the hour. Note
  that if more than 4-6 hours are requested this can take
  some time.
}
\examples{
data(wind)
s <- subsetOnSpeed(wind, 'R2', '<', 6.0)
s.avg <- buildHourlyAverages(s)
h <- c(0, 6, 12, 18)
s.hr <- subsetOnHour(s.avg, h)
m <- makeVectorMap(s.hr, 43.45, -113.15, 12, 'terrain')
}
