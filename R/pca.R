# Principal component analysis
# 
# Author: Andrie
###############################################################################


#' Extracts primary principal components from object of type `princomp`
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type princomp
#' @seealso \code{\link{cluster_data}}, \code{\link{plot_cluster}}
#' @param ... ignored
#' @export
cluster_data.princomp <- function(model, ...){
	data.frame(
			x = model$scores[, 1],
			y = model$scores[, 2]
	)
}

#' Extracts primary principal components from object of type `prcomp`
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type prcomp
#' @seealso \code{\link{cluster_data}}, \code{\link{plot_cluster}}
#' @param ... ignored
#' @export
cluster_data.prcomp <- function(model, ...){
	data.frame(
			x = model$x[, 1],
			y = model$x[, 2]
	)
}

#' Extracts primary principal components from object of type `factanal`
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type factanal
#' @seealso \code{\link{cluster_data}}, \code{\link{plot_cluster}}
#' @param ... ignored
#' @export
cluster_data.factanal <- function(model, ...){
	data.frame(
			x = model$loadings[, 1],
			y = model$loadings[, 2]
	)
}

