\name{get_cluster_data.dendrogram}
\alias{get_cluster_data.dendrogram}
\title{Extract data frame from dendrogram object for plotting using ggplot...}
\usage{get_cluster_data.dendrogram(x, type=c("rectangle", "triangle"), ...)}
\description{Extract data frame from dendrogram object for plotting using ggplot}
\details{Extract data frame from dendrogram object for plotting using ggplot}
\seealso{\code{\link{get_cluster_data}}, \code{\link{get_cluster_labels}}}
\arguments{\item{x}{object of class "dendrogram", e.g. the output of as.dendrogram()}
\item{type}{The type of plot, indicating the shape of the dendrogram.  "Rectangle" will draw
rectangular lines, while "triangle" will draw triangular lines.}
\item{...}{ignored}}
\examples{hc <- hclust(dist(USArrests), "ave")
d <- as.dendrogram(hc)
# Rectangular lines
ddata <- get_cluster_data(d, type="rectangle")
ggplot(ddata) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1)) + 
coord_flip() + scale_y_reverse(expand=c(0.2, 0))}
