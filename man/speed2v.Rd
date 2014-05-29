\name{speed2v}
\alias{speed2v}
\title{Convert speed/direction to v-component of wind}
\usage{
speed2v(s, d)
}
\arguments{
  \item{s}{wind speed}

  \item{d}{wind direction in degrees}
}
\value{
v-component of wind
}
\description{
\code{speed2v} returns v-component of wind
}
\details{
This fucntion takes a wind speed and direction and returns
the v-component of the wind. Direction must be supplied in
degrees.
}
\examples{
data(wind)
speed2v(wind$obs_speed[1], wind$obs_dir[1])
}

