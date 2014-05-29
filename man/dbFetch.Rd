\name{dbFetch}
\alias{dbFetch}
\title{Generic fetching function}
\usage{
dbFetch(db, query_string)
}
\arguments{
  \item{db}{wind database to query}

  \item{query_string}{query to submit to database}
}
\value{
dataframe
}
\description{
\code{dbFetch} returns a dataframe from an SQLite database
}
\details{
This fucntion returns a dataframe of data returned from
\code{query_string}
}

