\name{wnReadBiasData}
\alias{wnReadBiasData}
\title{Read bias.txt file create from writeBias.py}
\usage{
wnReadBiasData(fileName, type)
}
\arguments{
  \item{fileName}{full path to bias.txt}

  \item{type}{Type of WindNinja initializaiton used; p =
  point, w = weather}
}
\value{
dataframe that can be sent to wnBuildBiasDf()
}
\description{
\code{wnReadBiasData} reads bias.txt into a dataframe
}
\details{
Reads in a bias.txt file created by writeBias.py. The
bias.txt file must contain the following data in this
order: 'identifier', 'lat', 'lon', 'datetime', 'obs_speed',
'pred_speed', 'bias_speed', 'obs_dir', 'pred_dir',
'bias_dir', 'wx_speed', 'wx_bias_speed', 'wx_dir',
'wx_bias_dir'
}

