\name{cluster_data.Mclust}
\alias{cluster_data.Mclust}
\title{Extracts cluster allocation from object of type `Mclust`...}
\usage{\method{cluster_data}{Mclust} (model, ...)}
\description{Extracts cluster allocation from object of type `Mclust`}
\details{Extracts the cluster allocation from a \code{\link[mclust]{Mclust}} analysis into data.frame}
\seealso{\code{\link{cluster_data}}, \code{\link{plot_cluster}}}
\value{A data frame with the following columns:
\item{cluster}{Cluster allocation}}
\arguments{\item{model}{object of type Mclust}
\item{...}{ignored}}
\examples{require(mclust)
data(iris)
iris <- iris[, -5]
model <- Mclust(iris, 3)
str(cluster_data(model))}
