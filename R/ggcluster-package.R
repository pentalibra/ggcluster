# TODO: Add comment
# 
# Author: Andrie
###############################################################################


#' Tools for creating cluster plots, tree plots and dendrograms using ggplot.
#'
#' This is a set of tools for creating cluster plots, tree plots and dendrograms 
#' using ggplot()
#' 
#' The ggplot philosophy is to clearly separate data from the presentation.
#' Unfortunately not all cluster algorithms in R makes this clear distinction.

#' ggcluster creates a framework for extracting data and labels from
#' the various cluster algorithms.  There is one generic function, \code{\link{cluster_data}}
#' with speficic implementations for:
#' 
#' \itemize{
#' \item hclust: \code{\link{cluster_data.hclust}} 
#' \item dendrogram: \code{\link{cluster_data.dendrogram}} 
#' \item tree: \code{\link{cluster_data.tree}} 
#' \item kmeans: \code{\link{cluster_data.kmeans}} 
#' \item Mclust: \code{\link{cluster_data.Mclust}} 
#' }
#'
#' @name ggcluster-package
#' @aliases ggcluster
#' @docType package
#' @title Tools for creating cluster plots, tree plots and dendrograms using ggplot in [R]
#' @author Andrie de Vries \email{andrie.de.vries@@pentalibra.com}
#' @keywords package
#' @seealso \code{\link{cluster_data}}

NULL
