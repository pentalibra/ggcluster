## TODO: Add comment
## 
## Author: Andrie
################################################################################
#
#

#' Extracts cluster allocation from object of type `kmeans`
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type kmeans
#' @param ... ignored
#' @export
get_cluster_data.kmeans <- function(model, ...){
	data.frame(cluster=model$cluster)
}

#' Extracts cluster allocation from object of type `Mclust`
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type kmeans
#' @param ... ignored
#' @export
get_cluster_data.Mclust <- function(model, ...){
	data.frame(cluster=model$classification)
}


#' Extracts primary principal components from object of type `princomp`
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type princomp
#' @param ... ignored
#' @export
get_cluster_data.princomp <- function(model, ...){
	data.frame(
			x = model$scores[,1],
			y = model$scores[,2]
	)
}

#' Creates ellipsoid from dataframe containing x and y 
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param data A data frame containing at least three columns: x, y and cluster.
#' x and y represent the plotting coordinates, and cluster is the cluster allocation.
#' @param x Character string, indicating the x coordinate
#' @param y Character string, indicating the y coordinate
#' @param cluster Character string, indicating the cluster allocation
#' @export
#' @examples
#' data(iris)
#' d <- iris[, -5]
#' model <- kmeans(d, 3)
#' cdata <- get_cluster_data(model)
#' pc <- princomp(d)
#' pcdata <- get_cluster_data(pc)
#' eedata <- cbind(cdata, pcdata)
#' eldata <- get_ellipsoid_data(eedata)
#' 
#' ggplot() + 
#' 		geom_point(data=cbind(cdata, pcdata), 
#' 			aes(x=x, y=y, colour=factor(cluster))) +
#' 		geom_polygon(data=eldata, 
#' 			aes(x=x, y=y, colour=factor(cluster), group=cluster), 
#' 			alpha=0.1)
get_ellipsoid_data <- function(data, x="x", y="y", cluster="cluster"){
	get_ellipse <- function(clustn){
		sdata <- data[data[, cluster]==clustn, c(x, y)]
		edata <- as.matrix(sdata)
		ehull <- ellipsoidhull(edata)
		phull <- as.data.frame(predict(ehull))
		data.frame(x=phull$V1, y=phull$y, cluster=clustn)
	}
	ldply(unique(data[, cluster]), get_ellipse)
}


#' Convenience function to create a cluster plot using either kmeans or Mclust 
#' 
#' This function creates a simple ggplot object using the data and cluster model.  Since it 
#' is not aesthetically perfect, the user migth use it as a template to create more
#' customized plots. 
#' 
#' @param data a data frame to be used in clustering
#' @param model a cluster model of class kmeans or Mclust
#' @export
#' @examples
#' data(iris)
#' d <- iris[, -5]
#' # Using kmeans for the clustering
#' model <- kmeans(d, 3)
#' plot_cluster(d, model)
#' # Using Mclust for the clustering
#' model <- Mclust(d, 3)
#' plot_cluster(d, model)
plot_cluster <- function(data, model){
	pc <- princomp(data)
	cdata <- get_cluster_data(model)
	pcdata <- get_cluster_data(pc)
	eedata <- cbind(cdata, pcdata)
	eldata <- get_ellipsoid_data(eedata)
	
	ggplot() + 
			geom_point(data=cbind(cdata, pcdata), aes(x=x, y=y, colour=factor(cluster))) +
			geom_polygon(data=eldata, aes(x=x, y=y, colour=factor(cluster), group=cluster), alpha=0.1)
}


#data(iris)
#d <- iris[, -5]
#model <- kmeans(d, 3)
#cdata <- get_cluster_data(model)
#pc <- princomp(d)
#pcdata <- get_cluster_data(pc)
#eedata <- cbind(cdata, pcdata)
#eldata <- get_ellipsoid_data(eedata)
#
#ggplot() + 
#		geom_point(data=cbind(cdata, pcdata), aes(x=x, y=y, colour=factor(cluster))) +
#		geom_polygon(data=eldata, aes(x=x, y=y, colour=factor(cluster), group=cluster), alpha=0.1)



#
#
#plot_scree <- function(){
#	
#}
#
#plot_clusters <- function(){
#	
#}
#
#
#####################################################
## Cluster analysis scree plot
#####################################################
#
#ggClusterScree <- function(data, vecClusters=2:15){
#	wss <- data.frame(
#			clusters=vecClusters,
#			sums=laply(vecClusters, function(n) {sum(kmeans(data, centers=n)$withinss)})
#	)
#	
#	p <- ggplot(wss,aes(x=clusters,y=sums))
#	p <- p + geom_point() + geom_line(alpha=0.5) +
#			opts(title="Scree plot of cluster \"goodness of fit\"") +
#			xlab("Number of clusters") + ylab("Error (Sum of squares)")
#	return(p)
#}
#
#
#
#
#####################################################
## Principal component analysis
#####################################################
#
## Principal Components Analysis
## entering raw data and extracting PCs
## from the correlation matrix
#
#calc_PCA_components <- function(data){
#	
#	fit <- princomp(data, cor=TRUE)
#	
#	PCAcomponents <- data.frame(
#			Comp1 = fit$scores[,1],
#			Comp2 = fit$scores[,2]
#	)
#	return (PCAcomponents)
#}
#
#################################################################################
#
#make_PCA_graph <- function(data){
#	
#	fit <- princomp(data, cor=TRUE)
#	
#	fit_loadings <- function(x){
#		data.frame(
#				Comp.1 = x$loadings[,1],
#				Comp.2 = x$loadings[,2],
#				Angle  = atan(x$loadings[,2]/x$loadings[,1])/(2*pi)*360,
#				Dist   = (x$loadings[,2]^2+x$loadings[,1]^2)^0.5
#		)
#	}
#	
#	fit.l <- fit_loadings(fit)
#	fit.l$labeltext <- names(data)
#	print(fit.l)
#	
#	p <- ggplot(data=fit.l)
#	p <- p + geom_segment(aes(x=0,
#					y=0,
#					xend=Comp.1,
#					yend=Comp.2,
#			#                          alpha = Dist
#			),
#			arrow=arrow(length=unit(0.1,"cm")), colour="blue")
#	p <- p + geom_text(aes(x=Comp.1*1.1,
#					y=Comp.2*1.1,
#					label=labeltext,
#					angle=Angle,
#					#                       alpha=Dist,
#					hjust=1*Comp.1<0),
#			size=3,
#			colour="blue")
#	p <- p + scale_x_continuous(xLabel, limits=c(-01,  01), breaks=c(-1, 0, 1), minor.breaks=NULL)
#	p <- p + scale_y_continuous(yLabel, limits=c(-1, 01), breaks=c(-1, 0, 1), minor.breaks=NULL)
#	p <- p + geom_hline(aes(x=0), colour="grey80")
#	p <- p + geom_vline(aes(y=0), colour="grey80")
#	p <- p + coord_equal()
#	p <- p + opts(legend.position="none")
#	return(p)
#}
#
#
#####################################################
## Calculate ellipsoids that will best fit each cluster
#####################################################
#
#
#
#####################################################
## K-Means Cluster Analysis
#####################################################
#
#analyse_clusters <- function(data, PCAcomponents, nclusters){
#	
#	fit <- kmeans(data, nclusters) # n cluster solution
#	
#	# append cluster assignment
#	data$clust <- as.factor(LETTERS[fit$cluster])
#	data$Comp1 <- PCAcomponents$Comp1
#	data$Comp2 <- PCAcomponents$Comp2
#	
#	
#	
#	ellipsedata <- NULL
#	for (i in 1:nclusters)
#	{
#		e <- as.matrix(subset(data,clust==LETTERS[i],select=c(Comp1,Comp2)))
#		e <- ellipsoidhull(e)
#		e <- as.data.frame(predict(e))
#		e <- cbind(e, cluster=LETTERS[i])
#		ellipsedata <- rbind(ellipsedata,e)
#	}
#	names(ellipsedata) <- c("x","y","clust")
#	
#	
#	####################################################
#	# Plot graphic
#	####################################################
#	
#	clust.names <- paste("Segment",LETTERS[1:nclusters],sep=" ")
#	
#	
#	
#	p <- ggplot(data, aes(x=Comp1, y=Comp2, group=clust, colour=factor(clust)))
#	#p <- p + stat_density2d(alpha=0.5)
#	p <- p + geom_point() + scale_colour_brewer("Segment", palette="Set1")
#	p <- p + geom_vline(xintercept=0, colour="blue") + geom_hline(yintercept=0, colour="blue")
#	#p <- p + opts(title="Cluster plot")
#	p <- p + xlab(xLabel)
#	p <- p + ylab(yLabel)
#	p <- p + geom_polygon(data=ellipsedata,aes(x=x,y=y,group=factor(clust)),alpha=0.1)
#	p <- p + coord_equal()
#	theme_null <- theme_update(
#			panel.grid.major = theme_blank(),
#			panel.grid.minor = theme_blank(),
#			axis.text.x = theme_blank(),
#			axis.text.y = theme_blank(),
#			axis.ticks = theme_blank()
#	#legend.position = "none"
#	)
#	
#	p <- p + theme_set(theme_null)
#	
#	return(list(
#					clusters=data$clust,
#					p=p
#			))
#}

