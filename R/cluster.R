## Cluster analysis helper functions
## 
## Author: Andrie
################################################################################
#
#

#' Extracts cluster allocation from object of type `kmeans`
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type kmeans
#' @param ... ignored
#' @seealso \code{\link{cluster_data}}, \code{\link{plot_cluster}}
#' @method cluster_data kmeans
#' @export
cluster_data.kmeans <- function(model, ...){
	data.frame(cluster=model$cluster)
}

#' Extracts cluster allocation from object of type `Mclust`
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type Mclust
#' @seealso \code{\link{cluster_data}}, \code{\link{plot_cluster}}
#' @param ... ignored
#' @method cluster_data Mclust
#' @export
cluster_data.Mclust <- function(model, ...){
	data.frame(cluster=model$classification)
}




#' Creates ellipsoid from dataframe containing x and y 
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param data A data frame containing at least three columns: x, y and cluster.
#' x and y represent the plotting coordinates, and cluster is the cluster allocation.
#' @param x Character string, indicating the x coordinate
#' @param y Character string, indicating the y coordinate
#' @param cluster Character string, indicating the cluster allocation
#' @seealso \code{\link{cluster_data}}, \code{\link{plot_cluster}}
#' @export
#' @examples
#' data(iris)
#' d <- iris[, -5]
#' model <- kmeans(d, 3)
#' cdata <- cluster_data(model)
#' pc <- princomp(d)
#' pcdata <- cluster_data(pc)
#' eedata <- cbind(cdata, pcdata)
#' eldata <- ellipsoid_data(eedata)
#' 
#' ggplot() + 
#' 		geom_point(data=cbind(cdata, pcdata), 
#' 			aes(x=x, y=y, colour=factor(cluster))) +
#' 		geom_polygon(data=eldata, 
#' 			aes(x=x, y=y, colour=factor(cluster), group=cluster), 
#' 			alpha=0.1)
ellipsoid_data <- function(data, x="x", y="y", cluster="cluster"){
	get_ellipse <- function(clustn){
		sdata <- data[data[, cluster]==clustn, c(x, y)]
		edata <- as.matrix(sdata)
		ehull <- ellipsoidhull(edata)
		phull <- as.data.frame(predict(ehull))
		data.frame(x=phull$V1, y=phull$y, cluster=clustn)
	}
	ldply(unique(data[, cluster]), get_ellipse)
}





