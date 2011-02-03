\name{get_cluster_labels.hclust}
\alias{get_cluster_labels.hclust}
\title{Extract labels from hclust object for plotting using ggplot...}
\usage{get_cluster_labels.hclust(x, type=c("rectangle", "triangle"), ...)}
\description{Extract labels from hclust object for plotting using ggplot}
\details{Extract labels from hclust object for plotting using ggplot}
\seealso{\code{\link{get_cluster_data}}, \code{\link{get_cluster_labels}}}
\arguments{\item{x}{object of class "hclust", e.g. the output of hclust()}
\item{type}{The type of plot, indicating the shape of the dendrogram.  "Rectangle" will draw
rectangular lines, while "triangle" will draw triangular lines.}
\item{...}{ignored}}
\examples{hc <- hclust(dist(USArrests), "ave")
hcdata <- get_cluster_data(hc, type="triangle")
hclabels <- get_cluster_labels(hc, type="triangle")
ggplot() + geom_segment(data=hcdata, aes(x=x0, y=y0, xend=x1, yend=y1)) +
geom_text(data=hclabels, aes(x=x, y=y, label=text), size=3, hjust=0) +
coord_flip() + scale_y_reverse(expand=c(0.2, 0))}
