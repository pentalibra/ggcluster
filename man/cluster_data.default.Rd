\name{cluster_data.default}
\alias{cluster_data.default}
\title{Extract cluster data from a model into a list of data frames.}
\usage{
  \method{cluster_data}{default} (model, ...)
}
\arguments{
  \item{model}{An object}

  \item{...}{ignored}
}
\description{
  The default method for cluster_data will return an error.
  Since the data from different cluster models are so
  varied, it is not possible to write a generic function to
  cater for any type of object.
}

