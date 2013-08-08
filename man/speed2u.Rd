\name{speed2u}
\alias{speed2u}
\title{Convert speed/direction to u-component of wind}
\usage{
  speed2u(s, d)
}
\arguments{
  \item{s}{wind speed}

  \item{d}{wind direction in degrees}
}
\value{
  u-component of wind
}
\description{
  \code{speed2u} returns u-component of wind
}
\details{
  This fucntion takes a wind speed and direction and
  returns the u-component of the wind. Direction must be
  supplied in degrees.
}
\examples{
data(wind)
speed2u(wind$obs_speed[1], wind$obs_dir[1])
}

