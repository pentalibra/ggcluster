\name{cluster_data.dendrogram}
\alias{cluster_data.dendrogram}
\title{Extract line segment and label data from dendrogram object.}
\usage{\method{cluster_data}{dendrogram} (model, type=c("rectangle",
    "triangle"), ...)}
\description{Extract line segment and label data from dendrogram object.}
\details{Extract line segment and label data from dendrogram object.  Results are stored in a
list of data frames containing line segment data and label data.}
\value{A list with the following elements:
\item{segments}{Line segment data}
\item{labels}{Label data}}
\seealso{\code{\link{cluster_data}}}
\arguments{\item{model}{object of class "dendrogram", e.g. the output of as.dendrogram()}
\item{type}{The type of plot, indicating the shape of the dendrogram.  "rectangle" will draw
rectangular lines, while "triangle" will draw triangular lines.}
\item{...}{ignored}}
\examples{hc <- hclust(dist(USArrests), "ave")
dhc <- as.dendrogram(hc)
# Rectangular lines
ddata <- cluster_data(dhc, type="rectangle")
ggplot(ddata$segments) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1)) + 
coord_flip() + scale_y_reverse(expand=c(0.2, 0))
# Triangular lines
ddata <- cluster_data(dhc, type="triangle")
ggplot(ddata$segments) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1))}
