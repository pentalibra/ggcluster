\name{plot_cluster}
\alias{plot_cluster}
\title{Convenience function to create a cluster plot using either kmeans or Mclust...}
\usage{plot_cluster(data, model, xLabel="Component 1", yLabel="Component 2",
    pca_function=princomp, ellipse_quantile=1, segment_names)}
\description{Convenience function to create a cluster plot using either kmeans or Mclust}
\details{This function creates a cluster plot of the cluster data point on the first two axes of a principal component analysis.  An ellipsoid-hull is fitted round each cluster.}
\arguments{\item{data}{a data frame to be used in clustering}
\item{model}{a cluster model of class kmeans or Mclust}
\item{xLabel}{x-axis label}
\item{yLabel}{y-axis label}
\item{pca_function}{The method for calculating principal components.  Must be either princomp or prcomp}
\item{ellipse_quantile}{The quantile for plotting ellipsoids around clusters.  For a value of 1, the ellipsoid will be an ellipsoid-hull, i.e. all points will be included.}
\item{segment_names}{Character vector with cluster names}}
\examples{data(iris)
iris <- iris[, -5]

# Using kmeans for the clustering
plot_cluster(iris, kmeans(iris, 3))

# Using Mclust for the clustering
require(mclust)
plot_cluster(iris, Mclust(iris, 3))}
