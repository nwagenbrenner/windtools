\name{facetLabeller}
\alias{facetLabeller}
\title{Relable facets for wind regimes}
\usage{
  facetLabeller(var, value)
}
\arguments{
  \item{var}{varible to rename}

  \item{value}{new name to use in label}
}
\value{
  value to use in new label
}
\description{
  \code{facetLabeller} renames facet lables
}
\details{
  Internal fucntion that returns a new name to use for a
  facet label. Labels are fixed for particular wind
  regimes. This function is called by \code{makeVectorMap}
}

