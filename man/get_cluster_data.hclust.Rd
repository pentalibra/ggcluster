\name{get_cluster_data.hclust}
\alias{get_cluster_data.hclust}
\title{Extract data frame from hclust object for plotting using ggplot...}
\usage{get_cluster_data.hclust(x, type=c("rectangle", "triangle"), ...)}
\description{Extract data frame from hclust object for plotting using ggplot}
\details{Extract data frame from hclust object for plotting using ggplot}
\seealso{\code{\link{get_cluster_data}}, \code{\link{get_cluster_labels}}}
\arguments{\item{x}{object of class "hclust", e.g. the output of hclust()}
\item{type}{The type of plot, indicating the shape of the dendrogram.  "Rectangle" will draw
rectangular lines, while "triangle" will draw triangular lines.}
\item{...}{ignored}}
\examples{hc <- hclust(dist(USArrests), "ave")
# Rectangular lines
hcdata <- get_cluster_data(hc, type="rectangle")
ggplot(hcdata) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1)) + 
coord_flip() + scale_y_reverse(expand=c(0.2, 0))
# Triangular lines
hcdata <- get_cluster_data(hc, type="triangle")
ggplot(hcdata) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1))}
