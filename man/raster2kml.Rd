\name{raster2kml}
\alias{raster2kml}
\title{Write a kml from a raster object}
\usage{
  raster2kml(r, outfile)
}
\arguments{
  \item{r}{raster object}

  \item{outfile}{output file name}
}
\description{
  \code{raster2kml} writes a kml from a raster object
}
\details{
  This fucntion writes a kml representation of a raster
  object.
}
\examples{
r <- raster(ncol=10, nrow=10)
values(r) <- 1:ncell(r)
extent(r)<-c(-113.5732, -112.4852, 43.04848, 43.81728)
raster2kml(r, 'test_kml')
}

