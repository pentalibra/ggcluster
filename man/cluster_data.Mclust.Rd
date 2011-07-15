\name{cluster_data.Mclust}
\alias{cluster_data.Mclust}
\title{Extracts cluster allocation from object of type `Mclust`}
\usage{
  \method{cluster_data}{Mclust} (model, ...)
}
\arguments{
  \item{model}{object of type Mclust}

  \item{...}{ignored}
}
\value{
  A data frame with the following columns:
  \item{cluster}{Cluster allocation}
}
\description{
  Extracts the cluster allocation from a
  \code{\link[mclust]{Mclust}} analysis into a data.frame
}
\examples{
require(mclust)
data(iris)
iris <- iris[, -5]
model <- Mclust(iris, 3)
str(cluster_data(model))
}
\seealso{
  \code{\link{cluster_data}}, \code{\link{plot_cluster}}
}

