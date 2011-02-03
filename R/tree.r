# Plots tree object in ggplot2


#' Extract data frame from tree object for plotting using ggplot
#' 
#' Extract data frame from tree object for plotting using ggplot
#' 
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @export
#' @seealso \code{\link{get_cluster_labels}}
#' @examples
#' data(cpus, package="MASS")
#' cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
#' tree_data <- get_cluster_data(cpus.ltr)
#' tree_label_data <- get_cluster_labels(cpus.ltr)
#' leaf_label_data <- get_data_tree_leaf_labels(cpus.ltr)
#' ggplot(tree_data) +
#' 	geom_segment(aes(x=x, y=y, xend=xend, yend=yend, size=n), colour="blue", alpha=0.5) +
#' 	scale_size("n", to=c(0, 3)) +
#' 	geom_text(data=tree_label_data, aes(x=x, y=y, label=label), vjust=-0.5, size=4) +
#' 	geom_text(data=leaf_label_data, aes(x=x, y=y, label=label), vjust=0.5, size=3)
get_cluster_data.tree <- function(model, ...){
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

#' Extract labels data frame from tree object for plotting using ggplot
#' 
#' Extract labels data frame from tree object for plotting using ggplot
#' 
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @export
#' @seealso \code{\link{get_cluster_data}}
get_cluster_labels.tree <- function(model, ...){
  require(tree)
  # Uses tree:::treeco to extract data frame of plot locations
  xy <- tree:::treeco(model)
  label <- model$frame$var
  sleft  <- model$frame$splits.cutleft
  sright <- model$frame$splits.right

  # Lines copied from tree:::treepl
  x <- xy$x
  y <- xy$y
  node = as.numeric(row.names(model$frame))
  parent <- match((node%/%2), node)
  sibling <- match(ifelse(node%%2, node - 1L, node + 1L), node)

  data <- data.frame(x=x, y=y, label=label)
  data <- data[data$label != "<leaf>",]
  data
}

#' Extract labels data frame from tree object for plotting using ggplot
#' 
#' Extract labels data frame from tree object for plotting using ggplot
#' 
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @export
#' @seealso \code{\link{get_cluster_data}}
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


