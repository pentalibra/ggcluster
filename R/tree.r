# Plots tree object in ggplot2

#' Extract data from tree object for plotting using ggplot.
#' 
#' Extracts data to plot line segments and labels from a tree() object.  This data can
#' then be manipulated or plotted, e.g. using ggplot().
#' 
#' Returns a list of three data frames:
#' 
#' \itemize{
#' \item {$segments}{a data frame containing the line segment data}
#' \item {$labels}{a data frame containing the label text data}
#' \item {$leaf_labels}{a data frame containing the leaf label text data}
#' }
#' 
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @method cluster_data tree
#' @export
#' @return a list
#' @seealso \code{\link{cluster_data}}
#' @examples
#' data(cpus, package="MASS")
#' cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
#' tree_data <- cluster_data(cpus.ltr)
#' ggplot(tree_data$segments) +
#' 	geom_segment(aes(x=x, y=y, xend=xend, yend=yend, size=n), 
#' 		colour="blue", alpha=0.5) +
#' 	scale_size("n", to=c(0, 3)) +
#' 	geom_text(data=tree_data$labels, 
#' 		aes(x=x, y=y, label=label), vjust=-0.5, size=4) +
#' 	geom_text(data=tree_data$leaf_labels, 
#' 		aes(x=x, y=y, label=label), vjust=0.5, size=3)
cluster_data.tree <- function(model, ...){
	require(tree)
	labels <- tree_labels(model, ...)
	list(
			segments = cluster_segments.tree(model, ...),
			labels = labels$labels,
			leaf_labels = labels$leaf_labels
			)
}



#' Extract data frame from tree object for plotting using ggplot.
#' 
#' Extract data frame from tree object for plotting using ggplot
#' 
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @keywords internal
#' @seealso \code{\link{cluster_data.tree}}
#' @examples
#' data(cpus, package="MASS")
#' cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
#' tree_data <- cluster_data(cpus.ltr)
#' ggplot() +
#' 	geom_segment(data=tree_data$segments, 
#'    aes(x=x, y=y, xend=xend, yend=yend, size=n), 
#' 		colour="blue", alpha=0.5) +
#' 		scale_size("n", to=c(0, 3)) +
#' 	geom_text(data=tree_data$labels, 
#' 		aes(x=x, y=y, label=label), vjust=-0.5, size=4) +
#' 	geom_text(data=tree_data$leaf_labels, 
#' 		aes(x=x, y=y, label=label), vjust=0.5, size=3)
cluster_segments.tree <- function(model, ...){
  require(tree)
	# Uses tree:::treeco to extract data frame of plot locations
	xy <- tree:::treeco(model)
	n <- model$frame$n
	
	# Lines copied from tree:::treepl
	x <- xy$x
	y <- xy$y
	node = as.numeric(row.names(model$frame))
	parent <- match((node%/%2), node)
	sibling <- match(ifelse(node%%2, node - 1L, node + 1L), node)
	
	linev <- data.frame(x=x, y=y, xend=x, yend=y[parent], n=n)
	lineh <- data.frame(x=x[parent], y=y[parent], xend=x, yend=y[parent], n=n)
	
	rbind(linev[-1,], lineh[-1,])
}

#' Extract labels data frame from tree object for plotting using ggplot.
#' 
#' Extract labels data frame from tree object for plotting using ggplot
#' 
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @return a list with two elements: $labels and $leaf_labels
#' @keywords internal
#' @seealso \code{\link{cluster_data}}
tree_labels <- function(model, ...){
  require(tree)
  # Uses tree:::treeco to extract data frame of plot locations
  xy <- tree:::treeco(model)
  label <- model$frame$var
	yval  <- model$frame$yval
	sleft  <- model$frame$splits.cutleft
  sright <- model$frame$splits.right

  # Lines copied from tree:::treepl
  x <- xy$x
  y <- xy$y
  node = as.numeric(row.names(model$frame))
  parent <- match((node%/%2), node)
  sibling <- match(ifelse(node%%2, node - 1L, node + 1L), node)

	# Extract labels
  data <- data.frame(x=x, y=y, label=label)
  data <- data[data$label != "<leaf>",]
	labels <- as.data.frame(data)
	
	# Extract leaf labels
	data <- data.frame(x, y, label, yval)
	data <- data[data$label == "<leaf>",]
	data$label <- round(data$yval, 2)
	leaf_labels <- as.data.frame(data)
	
	list(
			labels      = labels,
			leaf_labels = leaf_labels
	)
}

#' Extract labels data frame from tree object for plotting using ggplot.
#' 
#' Extract labels data frame from tree object for plotting using ggplot
#' 
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @keywords internal
#' @seealso \code{\link{cluster_data}}
get_data_tree_leaf_labels <- function(model, ...){
  #require(tree)
  # Uses tree:::treeco to extract data frame of plot locations
  xy <- tree:::treeco(model)
  label <- model$frame$var
  yval  <- model$frame$yval
  sleft  <- model$frame$splits.cutleft
  sright <- model$frame$splits.right

  # Lines copied from tree:::treepl
  x <- xy$x
  y <- xy$y
  node = as.numeric(row.names(model$frame))
  parent <- match((node%/%2), node)
  sibling <- match(ifelse(node%%2, node - 1L, node + 1L), node)

  data <- data.frame(x, y, label, yval)
  data <- data[data$label == "<leaf>",]
  data$label <- round(data$yval, 2)
  data
}



################
# Example code #
################

#library(ggplot2)
#library(tree)


