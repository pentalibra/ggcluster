# ggcluster

This is a set of tools for creating cluster plots, tree plots and dendrograms in [R]

It supports functions to plot the results of:

* `tree()`
* `hclust()`
* `dendrogram()`
* `kmeans()`

## Important functions

ggcluster offers two generic functions to extract data and text from the various clustering
models:

* 'get_cluster_data()' will extract line segment data into a data frame.
* 'get_cluster_labels()' will extract text labels into a data frame.

The results of these functions can then be passed to `ggplot()` for plotting.

For example:

	hc <- hclust(dist(USArrests), "ave")
	dhcdata <- get_cluster_data(hc, type="rectangle")
	dhclabels <- get_cluster_labels(hc, type="rectangle")
	ggplot() + 
			geom_segment(data=dhcdata, aes(x=x0, y=y0, xend=x1, yend=y1)) +
			geom_text(data=dhclabels, aes(x=x, y=y, label=text), size=3, hjust=0) +
			coord_flip() + scale_y_reverse(expand=c(0.2, 0))




