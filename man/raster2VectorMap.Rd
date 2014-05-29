\name{raster2VectorMap}
\alias{raster2VectorMap}
\title{Make a vector map from wind speed and direction rasters}
\usage{
raster2VectorMap(df, lat, lon, zoom, maptype, scale_factor = 1000)
}
\arguments{
  \item{df}{dataframe}

  \item{lat}{center lat of Google Maps image}

  \item{lon}{center lon of Google Maps image}

  \item{zoom}{zoom for Google Maps image (1-20)}

  \item{maptype}{type of Google Maps image (terrain,
  hybrid, satellite, roadmap)}

  \item{scale_factor}{optional scaling factor to adjust
  vector length}
}
\value{
ggmap object representation of the wind field
}
\description{
\code{raster2VectorMap} returns a ggmap object of the
vector field
}
\details{
This fucntion returns a vector plot of the wind field
(supplied as 2 rasters: wind direction and wind speed)
overlayed on a static Google Maps image.
}

