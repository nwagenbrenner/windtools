\name{wnBuildBiasDf}
\alias{wnBuildBiasDf}
\title{Combine multiple bias.txt files into a single dataframe}
\usage{
  wnBuildBiasDf(dfList, nameList)
}
\arguments{
  \item{dfList}{list of dataframes to combine returned from
  wnReadBiasData()}

  \item{nameList}{list of weather forecasts used in
  WindNinja simulation}
}
\value{
  dataframe that can be used with other wintools functions
}
\description{
  \code{wnBuildBiasDf} convert list of bias dataframes into
  single dataframe
}
\details{
  Combines multiple bias.txt files that have been read in
  with wnReadBiasData() into a single dataframe that can be
  used with other windtools functions. Note that dfList and
  nameList MUST be in the same order.
}

