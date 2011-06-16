\name{cluster_data.tree}
\alias{cluster_data.tree}
\title{Extract data from tree object for plotting using ggplot.}
\usage{cluster_data.tree(model, ...)}
\description{Extract data from tree object for plotting using ggplot.}
\details{Extracts data to plot line segments and labels from a tree() object.  This data can
then be manipulated or plotted, e.g. using ggplot().

Returns a list of three data frames:

\itemize{
\item {$segments}{a data frame containing the line segment data}
\item {$labels}{a data frame containing the label text data}
\item {$leaf_labels}{a data frame containing the leaf label text data}
}}
\value{a list}
\seealso{\code{\link{cluster_data}}}
\arguments{\item{model}{object of class "tree", e.g. the output of tree()}
\item{...}{ignored}}
\examples{data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
tree_data <- cluster_data(cpus.ltr)
ggplot(tree_data$segments) +
geom_segment(aes(x=x, y=y, xend=xend, yend=yend, size=n), 
colour="blue", alpha=0.5) +
scale_size("n", to=c(0, 3)) +
geom_text(data=tree_data$labels, 
aes(x=x, y=y, label=label), vjust=-0.5, size=4) +
geom_text(data=tree_data$leaf_labels, 
aes(x=x, y=y, label=label), vjust=0.5, size=3)}
