\docType{package}
\name{ggcluster-package}
\alias{ggcluster}
\alias{ggcluster-package}
\title{Tools for creating cluster plots, tree plots and dendrograms using ggplot in [R]}
\description{
  Tools for creating cluster plots, tree plots and
  dendrograms using ggplot.
}
\details{
  This is a set of tools for creating cluster plots, tree
  plots and dendrograms using ggplot()

  The ggplot philosophy is to clearly separate data from
  the presentation.  Unfortunately not all cluster
  algorithms in R makes this clear distinction.  ggcluster
  creates a framework for extracting data and labels from
  the various cluster algorithms.

  \code{\link{cluster_data}} extracts data from several
  cluster algorithms.  It is a generic function with
  specific implementations for:

  \itemize{ \item hclust: \code{\link{cluster_data.hclust}}
  \item dendrogram: \code{\link{cluster_data.dendrogram}}
  \item tree: \code{\link{cluster_data.tree}} \item kmeans:
  \code{\link{cluster_data.kmeans}} \item Mclust:
  \code{\link{cluster_data.Mclust}} }

  \code{\link{plot_cluster}} offers a convenient way of
  plotting results.
}
\author{
  Andrie de Vries \email{andrie.de.vries@pentalibra.com}
}
\seealso{
  \code{\link{cluster_data}}
}
\keyword{package}

