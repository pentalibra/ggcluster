

#' Extracts cluster allocation from object of type `Mclust`
#' 
#' Extracts the cluster allocation from a \code{\link[mclust]{Mclust}} analysis into a data.frame
#' 
#' @param model object of type Mclust
#' @seealso \code{\link{cluster_data}}, \code{\link{plot_cluster}}
#' @param ... ignored
#' @method cluster_data Mclust
#' @return
#' A data frame with the following columns:
#' \item{cluster}{Cluster allocation}
#' @export
#' @examples 
#' require(mclust)
#' data(iris)
#' iris <- iris[, -5]
#' model <- Mclust(iris, 3)
#' str(cluster_data(model))
cluster_data.Mclust <- function(model, ...){
  data.frame(cluster=model$classification)
}

