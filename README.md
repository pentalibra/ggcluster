# ggcluster

This is a set of tools for creating cluster plots, tree plots and dendrograms in [R]

It supports functions to plot the results of:

* `tree()`
* `hclust()`
* `dendrogram()`
* `kmeans()`

## Important functions

ggcluster offers a generic function to extract data and text from the various clustering
models:

* `cluster_data()` extracts cluster information from the model object, e.g. cluster allocation, line segment data or label data.

The results of these functions can then be passed to `ggplot()` for plotting.

For example:

	hc <- hclust(dist(USArrests), "ave")
	hcdata <- cluster_data(hc, type="rectangle")
	p <- ggplot() + 
			geom_segment(data=hcdata$segments, aes(x=x0, y=y0, xend=x1, yend=y1)) +
			geom_text(data=hcdata$labels, aes(x=x, y=y, label=text)) +
			coord_flip() + scale_y_reverse(expand=c(0.2, 0))
	print(p) 




