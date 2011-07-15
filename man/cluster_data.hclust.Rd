\name{cluster_data.hclust}
\alias{cluster_data.hclust}
\title{Extract line segment and label data from hclust object.}
\usage{
  \method{cluster_data}{hclust} (model, type =
  c("rectangle", "triangle"), ...)
}
\arguments{
  \item{model}{object of class "hclust", e.g. the output of
  hclust()}

  \item{type}{The type of plot, indicating the shape of the
  dendrogram.  "rectangle" will draw rectangular lines,
  while "triangle" will draw triangular lines.}

  \item{...}{ignored}
}
\value{
  A list with the following elements: \item{segments}{Line
  segment data} \item{labels}{Label data}
}
\description{
  Results are stored in a list of data frames containing
  line segment data and label data.
}
\examples{
require(ggplot2)
hc <- hclust(dist(USArrests), "ave")
# Rectangular lines
hcdata <- cluster_data(hc, type="rectangle")
ggplot(hcdata$segments) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1)) +
coord_flip() + scale_y_reverse(expand=c(0.2, 0))
# Triangular lines
hcdata <- cluster_data(hc, type="triangle")
ggplot(hcdata$segments) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1))
}
\seealso{
  \code{\link{cluster_data}}
}

