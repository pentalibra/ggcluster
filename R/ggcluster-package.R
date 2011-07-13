# package documentation
# 
# Author: Andrie
###############################################################################


#' Tools for creating cluster plots, tree plots and dendrograms using ggplot.
#'
#' This is a set of tools for creating cluster plots, tree plots and dendrograms 
#' using ggplot()
#' 
#' The ggplot philosophy is to clearly separate data from the presentation.  Unfortunately not all cluster algorithms in R makes this clear distinction.  ggcluster creates a framework for extracting data and labels from the various cluster algorithms.
#' 
#' \code{\link{cluster_data}} extracts data from several cluster algorithms.  It is a generic function with specific implementations for:
#' 
#' \itemize{
#' \item hclust: \code{\link{cluster_data.hclust}} 
#' \item dendrogram: \code{\link{cluster_data.dendrogram}} 
#' \item tree: \code{\link{cluster_data.tree}} 
#' \item kmeans: \code{\link{cluster_data.kmeans}} 
#' \item Mclust: \code{\link{cluster_data.Mclust}} 
#' }
#'
#' \code{\link{plot_cluster}} offers a convenient way of plotting results.
#' 
#' @name ggcluster-package
#' @aliases ggcluster
#' @docType package
#' @title Tools for creating cluster plots, tree plots and dendrograms using ggplot in [R]
#' @author Andrie de Vries \email{andrie.de.vries@@pentalibra.com}
#' @keywords package
#' @seealso \code{\link{cluster_data}}
NULL

.onLoad <- function(libname, pkgname){
  message <- "The ggcluster package is experimental:\nWarning: Function syntax may change in future versions.\n"
  packageStartupMessage(message)
}  
