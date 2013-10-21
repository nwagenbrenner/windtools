\name{subsetOnDate}
\alias{subsetOnDate}
\title{Subset wind data on datetime criteria}
\usage{
  subsetOnDate(df, condition, dt)
}
\arguments{
  \item{df}{dataframe}

  \item{condition}{'<', '==', or '>'}

  \item{dt}{datetime (as a POSIXct object) to subset on}
}
\value{
  subsetted dataframe
}
\description{
  \code{subsetOnDate} returns subsetted dataframe
}
\details{
  This fucntion subsets the wind data frame based on a
  datetime criteria. dt must be a POSIXct object.
}
\examples{
data(wind)
dt <- as.POSIXct(strptime("2010-08-01 00:00:00", '\%Y-\%m-\%d \%H:\%M:\%S'))
s <- subsetOnDate(wind, '<', dt)
}

