# cluster_data function
# 
# Author: Andrie
###############################################################################


#' Extract cluster data from a model into a list of data frames.
#' 
#' This function provides a generic mechanism to extract relevant plotting data, 
#' typically line segments and labels, from a variety of cluster models.
#' 
#' In the case of dendrograms and tree models, the function will extract line 
#' segment data and labels.
#' 
#' In the case of kmeans or Mclust models, the function extracts the cluster allocation.
#' 
#' @param model object of type hclust, dendrogram, tree or kmeans
#' @param ... ignored
#' @export 
#' @return a list of data frames that contain the data appropriate to each cluster model
#' @seealso 
#' There are several implementations for specific cluster algorithms:
#'
#' \itemize{ 
#' \item \code{\link{cluster_data.hclust}}
#' \item \code{\link{cluster_data.dendrogram}}		 
#' \item \code{\link{cluster_data.tree}}	
#' \item \code{\link{cluster_data.kmeans}}
#' \item \code{\link{cluster_data.Mclust}}
#' }
cluster_data <- function(model, ...){
	UseMethod("cluster_data", model)
}

#' Extract cluster data from a model into a list of data frames.
#' 
#' The default method for cluster_data will return an error.  Since the data from different cluster models are so varied, it is not possible to write a generic function to cater for any type of object. 
#' 
#' @param model An object
#' @param ... ignored
#' @method cluster_data default
#' @export 
cluster_data.default <- function(model, ...){
  x <- class(model)
  stop(paste("No cluster_data method defined for class", x))
  return(NULL)
}
