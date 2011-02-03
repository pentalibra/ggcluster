\name{get_cluster_labels}
\alias{get_cluster_labels}
\title{Generic function to extract cluster labels from a model into a data frame...}
\usage{get_cluster_labels(model, ...)}
\description{Generic function to extract cluster labels from a model into a data frame}
\details{Generic function to extract cluster labels from a model into a data frame}
\seealso{There are several implementations for specific cluster alogorithms:

\code{\link{get_cluster_labels.hclust}},	
\code{\link{get_cluster_labels.dendrogram}},		 
\code{\link{get_cluster_labels.tree}},	
\code{\link{get_cluster_labels.kmeans}}

To extract the cluster data, use \code{\link{get_cluster_data}}}
\arguments{\item{model}{object of type hclust, dendrogram, tree or kmeans}
\item{...}{ignored}}
