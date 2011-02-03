\name{ggcluster-package}
\title{Tools for creating cluster plots, tree plots and dendrograms using ggplot in [R]}
\description{Tools for creating cluster plots, tree plots and dendrograms using ggplot}
\details{\tabular{ll}{
Package: \tab ggcluster\cr
Type: \tab Package\cr
Version: \tab 0.0-05\cr
Date: \tab 2011-02-03\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

This is a set of tools for creating cluster plots, tree plots and dendrograms 
using ggplot()

The ggplot philosophy is to clearly separate data from the presentation.
Unfortunately not all cluster algorithms in R makes this clear distinction.
This packages creates a framework for extracting data and labels from
the various cluster algorithms.  There are two generic functions:

\itemize{
\item \code{\link{get_cluster_data}}: Extracts data points
\item \code{\link{get_cluster_labels}}: Extract labels
}}
\alias{ggcluster}
\docType{package}
\author{Andrie de Vries \email{andrie.de.vries@pentalibra.com}}
\keyword{package}
\keyword{[R]}
\seealso{\code{\link{get_cluster_data}}, \code{\link{get_cluster_labels}}}
