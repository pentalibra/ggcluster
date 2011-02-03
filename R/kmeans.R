## TODO: Add comment
## 
## Author: Andrie
################################################################################
#
#
#### Respondent drop-out analysis
#
#library(cluster)
#library(ggplot2)
#library(plyr)
##library(directlabels)
#

#' Generic function to extract cluster labels from a model into a data frame
#' 
#' Generic function to extract cluster labels from a model into a data frame
#' 
#' @param model object of type hclust, dendrogram, tree or kmeans
#' @param ... ignored
#' @export
get_cluster_data.kmeans <- function(model, ...){
	require(cluster)
}
#
#get_ellipsoid_data <- function(){
#	
#}
#
#get_components_data <- function(){
#	
#}
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
#
#analyse_cluster_profiles <- function(data, clusters){
##  cmeans <- ldply(data[, 1:13], function(x)tapply(x, data$clust, mean))
#	data$clust <- clusters
#	cmeans <- ldply(data[, -ncol(data)], function(x)tapply(x, data$clust, mean))
#	#rownames(cmeans) <- cmeans$.id
#	#cmeans$.id <- NULL
#	
#	sortClust <- function(data, clust){
#		x <- data[, c(1, 1+clust)]
#		x[order(x[, 2], decreasing=TRUE), ]
#	}
#	
#	nclusters <- length(unique(clusters))
#	ClusterMeans <- cmeans[,1]
#	
#	for (i in 1:nclusters){
#		ClusterMeans <- cbind(
#				ClusterMeans,
#				sortClust(cmeans, i)
#		)
#	}
#	return(ClusterMeans[,-1])
#}
#
#
#################################################################################
## Main processing
#################################################################################
#
## Global variables - some of them to be determined as result of analysis
#
## Number of clusters
#nclusters <- 5
#
#xLabel <- "<---   Luxury   --->"
#yLabel <- "<---   Essentials   --->"
#
#set.seed(12345)
#
#
#
#####################################################
## Determine number of clusters
#####################################################
#
##cdata <- kd[, 2:15] # Remove resp id, crossbreak and weight columns
#
#standardise <- function(x){
#	x/mean(x)
#}
#
#cdata <- kd[, 2:15]
##cdata <- cdata -4
##cdata <- cdata[, names(cdata)!="new.product"]
##cdata <- cdata[, names(cdata)!="slowly"]
##cdata <- cdata[, names(cdata)!="essentials"]
##cdata <- cdata[, names(cdata)!="familiar"]
##sdata <- as.data.frame(llply(cdata, standardise))
#sdata <- as.data.frame(scale(cdata))
#
#
#p <- ggClusterScree(sdata)
##print(p)
#
#PCAcomponents <- calc_PCA_components(sdata)
#
#p <- make_PCA_graph(sdata)
#print(p)
#
#cres3 <- analyse_clusters(sdata, PCAcomponents, 3)
#cres3_profiles <- analyse_cluster_profiles(sdata, cres3$clusters)
#print(cres3$p)
#
#cres4 <- analyse_clusters(sdata, PCAcomponents, 4)
#cres4_profiles <- analyse_cluster_profiles(sdata, cres4$clusters)
#print(cres4$p)
#
#cres5 <- analyse_clusters(sdata, PCAcomponents, 5)
#cres5_profiles <- analyse_cluster_profiles(sdata, cres5$clusters)
#print(cres5$p)
#
#cres6 <- analyse_clusters(sdata, PCAcomponents, 6)
#cres6_profiles <- analyse_cluster_profiles(sdata, cres6$clusters)
#print(cres6$p)
#
## Export profiles to Excel
#write.excel(cres4_profiles)
#write.excel(cres5_profiles)
#
#
## Add cluster centers to original dataset
#
#results <- data.frame(
#		RESPID = kd$RESPID,
#		Cluster3 = cres3$clusters,
#		Cluster4 = cres4$clusters,
#		Cluster5 = cres5$clusters,
#		Cluster6 = cres6$clusters,
#		Factor_x = PCAcomponents$Comp1,
#		Factor_y = PCAcomponents$Comp2
#)
#
#write.csv(results, paste(path, "\\data\\Segmentation results.csv", sep=""))
#write.csv(removed_responses, paste(path, "\\data\\Removed responses.csv", sep=""))
#write.csv(na_responses, paste(path, "\\data\\Incomplete responses.csv", sep=""))
#
#### Missing data analysis
##
###write.excel(kdall[-which(complete.cases(kdall)),])
##
###
##
##fullprofile <- merge(fulldata, results, by="RESPID", all.x=TRUE)
##
##tapply(as.numeric(fullprofile$Gender), fullprofile$Cluster4, mean)
###tapply(as.numeric(fullprofile$Age), fullprofile$Cluster4, mean)
##tapply(as.numeric(fullprofile$Languages), fullprofile$Cluster4, mean)
##tapply(as.numeric(fullprofile$Countries_historic), fullprofile$Cluster4, mean)
##
##

