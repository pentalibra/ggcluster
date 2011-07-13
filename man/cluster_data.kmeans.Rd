\name{cluster_data.kmeans}
\alias{cluster_data.kmeans}
\title{Extracts cluster allocation from object of class `kmeans`.}
\usage{\method{cluster_data}{kmeans} (model, ...)}
\description{Extracts cluster allocation from object of class `kmeans`.}
\details{Extracts the cluster allocation from a \code{\link{kmeans}} analysis into data.frame}
\seealso{\code{\link{cluster_data}}, \code{\link{plot_cluster}}}
\value{A data frame with the following columns:
\item{cluster}{Cluster allocation}}
\arguments{\item{model}{object of type kmeans}
\item{...}{ignored}}
\examples{data(iris)
iris <- iris[, -5]
model <- kmeans(iris, 3)
str(cluster_data(model))}
