\name{plot_cluster}
\alias{plot_cluster}
\title{Convenience function to create a cluster plot using either kmeans or Mclust...}
\usage{plot_cluster(data, model, pca=princomp)}
\description{Convenience function to create a cluster plot using either kmeans or Mclust}
\details{This function creates a simple ggplot object using the data and cluster model.  Since it is not aesthetically perfect, the user might use it as a template to create more customised plots.}
\arguments{\item{data}{a data frame to be used in clustering}
\item{model}{a cluster model of class kmeans or Mclust}
\item{pca}{The method for calculating principal components.  Must be either princomp or prcomp}}
\examples{data(iris)
iris <- iris[, -5]

# Using kmeans for the clustering
plot_cluster(iris, kmeans(iris, 3))

# Using Mclust for the clustering
plot_cluster(iris, Mclust(iris, 3))}
