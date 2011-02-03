\name{get_cluster_data.tree}
\alias{get_cluster_data.tree}
\title{Extract data frame from tree object for plotting using ggplot...}
\usage{get_cluster_data.tree(model, ...)}
\description{Extract data frame from tree object for plotting using ggplot}
\details{Extract data frame from tree object for plotting using ggplot}
\seealso{\code{\link{get_cluster_labels}}}
\arguments{\item{model}{object of class "tree", e.g. the output of tree()}
\item{...}{ignored}}
\examples{data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
tree_data <- get_cluster_data(cpus.ltr)
tree_label_data <- get_cluster_labels(cpus.ltr)
leaf_label_data <- get_data_tree_leaf_labels(cpus.ltr)
ggplot(tree_data) +
geom_segment(aes(x=x, y=y, xend=xend, yend=yend, size=n), 
colour="blue", alpha=0.5) +
scale_size("n", to=c(0, 3)) +
geom_text(data=tree_label_data, 
aes(x=x, y=y, label=label), vjust=-0.5, size=4) +
geom_text(data=leaf_label_data, 
aes(x=x, y=y, label=label), vjust=0.5, size=3)}
