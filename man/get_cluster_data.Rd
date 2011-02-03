\name{get_cluster_data}
\alias{get_cluster_data}
\title{Generic function to extract cluster data from a model into a data frame...}
\usage{get_cluster_data(model, ...)}
\description{Generic function to extract cluster data from a model into a data frame}
\details{Generic function to extract cluster data from a model into a data frame}
\seealso{There are several implementations for specific cluster algorithms:\cr
\code{\link{get_cluster_data.hclust}}
\cr	
\code{\link{get_cluster_data.dendrogram}}		 
\cr	
\code{\link{get_cluster_data.tree}}	
\cr	
\code{\link{get_cluster_data.kmeans}}
\cr	
To extract the cluster labels, use \code{\link{get_cluster_labels}}}
\arguments{\item{model}{object of type hclust, dendrogram, tree or kmeans}
\item{...}{ignored}}
