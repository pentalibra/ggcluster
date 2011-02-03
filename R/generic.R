# TODO: Add comment
# 
# Author: Andrie
###############################################################################


#' Generic function to extract cluster data from a model into a data frame
#' 
#' Generic function to extract cluster data from a model into a data frame
#' 
#' @param model object of type hclust, dendrogram, tree or kmeans
#' @param ... ignored
#' @seealso 
#' There are several implementations for specific cluster algorithms:\cr
#' \code{\link{get_cluster_data.hclust}}
#' \cr	
#' \code{\link{get_cluster_data.dendrogram}}		 
#' \cr	
#' \code{\link{get_cluster_data.tree}}	
#' \cr	
#' \code{\link{get_cluster_data.kmeans}}
#' \cr	
#' To extract the cluster labels, use \code{\link{get_cluster_labels}}
#' @export
get_cluster_data <- function(model, ...){
	UseMethod("get_cluster_data", model)
}

#' Generic function to extract cluster labels from a model into a data frame
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type hclust, dendrogram, tree or kmeans
#' @param ... ignored
#' @seealso 
#' There are several implementations for specific cluster alogorithms:
#' 
#' \code{\link{get_cluster_labels.hclust}},	
#' \code{\link{get_cluster_labels.dendrogram}},		 
#' \code{\link{get_cluster_labels.tree}},	
#' \code{\link{get_cluster_labels.kmeans}}
#' 
#' To extract the cluster data, use \code{\link{get_cluster_data}}
#' @export
get_cluster_labels <- function(model, ...){
	UseMethod("get_cluster_labels", model)
}
