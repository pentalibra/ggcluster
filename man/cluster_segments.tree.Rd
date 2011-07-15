\name{cluster_segments.tree}
\alias{cluster_segments.tree}
\title{Extract data frame from tree object for plotting using ggplot.}
\usage{
  cluster_segments.tree(model, ...)
}
\arguments{
  \item{model}{object of class "tree", e.g. the output of
  tree()}

  \item{...}{ignored}
}
\description{
  Extract data frame from tree object for plotting using
  ggplot
}
\examples{
require(tree)
require(ggplot2)
data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
tree_data <- cluster_data(cpus.ltr)
ggplot() +
geom_segment(data=tree_data$segments,
aes(x=x, y=y, xend=xend, yend=yend, size=n),
colour="blue", alpha=0.5) +
scale_size("n", to=c(0, 3)) +
geom_text(data=tree_data$labels,
aes(x=x, y=y, label=label), vjust=-0.5, size=4) +
geom_text(data=tree_data$leaf_labels,
aes(x=x, y=y, label=label), vjust=0.5, size=3)
}
\seealso{
  \code{\link{cluster_data.tree}}
}
\keyword{internal}

