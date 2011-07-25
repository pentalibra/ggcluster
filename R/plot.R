# plot functions
# 
# Author: Andrie
###############################################################################

#' Calculate pca fit loadings.
#' 
#' @param x Cluster model, e.g. the results of kmeans
#' @param labels_at_edge Indicates whether labels are at edge of graph or next to poit
#' @param explode Explode multiplication factor
#' @keywords Internal
pca_fit_loadings <- function(x, labels_at_edge, explode){
  if(class(x)=="prcomp"){
    Comp1 <- x$rotation[,1]
    Comp2 <- -x$rotation[,2]
  } else if(class(x)=="princomp" | class(x)=="factanal" | class(x)=="loadings"){
    Comp1 <- x$loadings[,1]
    Comp2 <- x$loadings[,2]
#   } else if(class(x)=="factanal" | class(x)=="loadings"){
#     Comp.1 <- x$loadings[, 1]
#     Comp.2 <- x$loadings[, 2]
  } else {
    stop("In plot_pca(), arguments to fit_loadings() must be of class prcomp or princomp")
  }
  angle  = atan2(Comp2, Comp1)
  if(labels_at_edge){
    data.frame(
        Comp1 = Comp1,
        Comp2 = Comp2,
        Comp1text = cos(angle),
        Comp2text = sin(angle),
        Angle  = atan(Comp2 / Comp1)/(2*pi)*360,
        Dist   = (Comp2^2 + Comp1^2)^0.5,
        Comp1explode = Comp1 * explode,
        Comp2explode = Comp2 * explode
    )
  } else {
    data.frame(
        Comp1 = Comp1,
        Comp2 = Comp2,
        Angle  = atan(Comp2 / Comp1)/(2*pi)*360,
        Dist   = (Comp2^2 + Comp1^2)^0.5,
        Comp1explode = Comp1 * explode,
        Comp2explode = Comp2 * explode
    )
  }
}

###############################################################################

#' Create principal components analysis plot.
#' 
#' @param data The data used for clustering
#' @param labeltext The labels to use for each column in the data.  Defaults to names(data).
#' @param xLabel x-axis label
#' @param yLabel y-axis label
#' @param pca_function The function to use for principal component analysis.  This could be \code{\link{princomp}}, \code{\link{prcomp}} or \code{\link{factanal}}.
#' @param labels_at_edge If TRUE, labels are printed at at edge of plot, otherwise labels are printed next to each point.
#' @param explode A multiplication factor that determines the distance of each label from the point.  If over-plotting of labels is a problem, then use a larger explode factor.
#' @param textsize Text label size passed to \code{\link[ggplot2]{geom_text}} 
#' @usage
#' plot_pca(
#' data, 
#' labeltext = names(data), 
#' xLabel = "Principal component 1", 
#' yLabel = "Principal component 2", 
#' pca_function = princomp, 
#' labels_at_edge = TRUE,
#' explode = 1.1, 
#' textsize = 3)
#' @export 
plot_pca <- function(data,
    labeltext = names(data),
    xLabel="Principal component 1", 
    yLabel="Principal component 2",
    pca_function=princomp,
    labels_at_edge = TRUE,
    explode=1.1,
    textsize=3){
  
  
  if(identical(pca_function, factanal)){
    fit <- factanal(data, factors=5)
  } else {
    fit <- pca_function(data, cor=TRUE)
  } 
  fit.l <- pca_fit_loadings(x=fit, labels_at_edge = labels_at_edge, explode=explode)
  fit.l$labeltext <- labeltext
# print(fit.l)
  
  p <- ggplot(data=fit.l)
  p <- p + geom_segment(aes_string(x=0, y=0, xend="Comp1", yend="Comp2", alpha="Dist"),
      arrow=arrow(length=unit(0.1,"cm")))#, colour="grey50")
  if(labels_at_edge){
    p <- p + geom_text(aes_string(
            x     = "Comp1text",
            y     = "Comp2text",
            label = "labeltext",
            angle = "Angle",
            alpha = "Dist",
            hjust = "1*Comp1>0"),
        size=3,
        colour="blue")
  } else {
    p <- p + geom_text(aes_string(
            x     = "Comp1explode",
            y     = "Comp2explode",
            label = "labeltext",
            angle = "Angle",
            alpha = "Dist",
            hjust = "1*Comp1<0"),
        size=3,
        colour="blue")
  }
  p <- p +
      theme_bw() +
      scale_x_continuous(limits=c(-01,  01), breaks=c(-1, 0, 1), minor.breaks=NULL) +
      scale_y_continuous(limits=c(-1, 01), breaks=c(-1, 0, 1), minor.breaks=NULL) +
      geom_hline(aes(x=0), colour="grey80") +
      geom_vline(aes(y=0), colour="grey80") +
      coord_equal() +
      xlab(xLabel) + ylab(yLabel) +
      opts(legend.position="none")
      
  return(p)
  
}

###############################################################################

#' Calculates ellipsoid data for use in plot_cluster.
#' 
#' @param data
#' @param x
#' @param y
#' @param cluster
#' @param ellipse_quantile
#' @keywords internal
.ellipsoid_data <- function(data, x="x", y="y", cluster="cluster", ellipse_quantile=0.67){
  require(plyr)
  require(cluster) #for ellipsoidhull and predict
  centroid_distance <- function(xr, xc){sqrt(sum((xc-xr)^2))}
  get_ellipse <- function(clustn){
    sdata <- data[data[, cluster]==clustn, c(x, y)]
    edata <- as.matrix(sdata)
    # Calculate centroid
    centroid <- c(mean(sdata$x), mean(sdata$y))
    centroid_distance <- laply(
        seq(nrow(edata)), 
        function(i)centroid_distance(edata[i, ], centroid))
    ehull <- ellipsoidhull(edata[centroid_distance <= quantile(centroid_distance, ellipse_quantile), ])
    phull <- as.data.frame(predict(ehull))
    data.frame(x=phull$V1, y=phull$y, cluster=clustn)
  }
  ldply(unique(data[, cluster]), get_ellipse)
}

###############################################################################

#' Convenience function to create a cluster plot using either kmeans or Mclust. 
#' 
#' This function creates a cluster plot of the cluster data point on the first two axes of a principal component analysis.  An ellipsoid-hull is fitted round each cluster. 
#' 
#' @param data a data frame to be used in clustering
#' @param model a cluster model of class kmeans or Mclust
#' @param xLabel x-axis label
#' @param yLabel y-axis label
#' @param pca_function The method for calculating principal components.  Must be either princomp or prcomp
#' @param ellipse_quantile The quantile for plotting ellipsoids around clusters.  For a value of 1, the ellipsoid will be an ellipsoid-hull, i.e. all points will be included.
#' @param segment_names Character vector with cluster names
#' @export
#' @examples
#' data(iris)
#' iris <- iris[, -5]
#' 
#' # Using kmeans for the clustering
#' plot_cluster(iris, kmeans(iris, 3))
#' 
#' # Using Mclust for the clustering
#' require(mclust)
#' plot_cluster(iris, Mclust(iris, 3))
plot_cluster <- function(
    data, 
    model, 
    xLabel="Component 1", 
    yLabel="Component 2", 
    pca_function=princomp,
    ellipse_quantile=1,
    segment_names=NULL){
  
  require(ggplot2)
  pc <- match.fun(pca_function)(data)
  cdata  <- cluster_data(model)
  pcdata <- cluster_data(pc)
  #str(cdata)
  #str(pcdata)
  eedata <- cbind(cdata, pcdata)
  if(!is.null(segment_names)){
    eedata$cluster <- segment_names[eedata$cluster]
  }
  eldata <- .ellipsoid_data(eedata, ellipse_quantile=ellipse_quantile)
  
  ggplot() + 
      geom_point(
          data = eedata, 
          aes_string(
              x = "x", 
              y = "y", 
              colour = "factor(cluster)", 
              group = "factor(cluster)")) + 
      geom_polygon(
          data = eldata, 
          aes_string(
              x = "x", 
              y = "y", 
              group = "factor(cluster)", 
              colour = "factor(cluster)"), 
          alpha = 0.1) +
      labs(
          x=xLabel,
          y=yLabel,
          colour="Cluster") +
      theme_bw()
}



#plot_cluster <- function(data, model, pca=princomp){
#	pc <- pca(data)
#	cdata <- cluster_data(model)
#	pcdata <- cluster_data(pc)
#	eedata <- cbind(cdata, pcdata)
#	eldata <- .ellipsoid_data(eedata)
#	
#	ggplot() + 
#			geom_point(
#					data=eedata, 
#					aes(x=x, y=y, colour=factor(cluster))) +
#			geom_polygon(
#					data=eldata, 
#					aes(x=x, y=y, colour=factor(cluster), group=cluster), alpha=0.1)
#}

