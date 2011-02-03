\name{get_ellipsoid_data}
\alias{get_ellipsoid_data}
\title{Creates ellipsoid from dataframe containing x and y...}
\usage{get_ellipsoid_data(data, x="x", y="y", cluster="cluster")}
\description{Creates ellipsoid from dataframe containing x and y}
\details{Generic function to extract cluster labels from a model into a data frame}
\arguments{\item{model}{object of type princomp}
\item{...}{ignored}}
\examples{data(iris)
d <- iris[, -5]
model <- kmeans(d, 3)
cdata <- get_cluster_data(model)
pc <- princomp(d)
pcdata <- get_cluster_data(pc)
eedata <- cbind(cdata, pcdata)
eldata <- get_ellipsoid_data(eedata)

ggplot() + 
geom_point(data=cbind(cdata, pcdata), aes(x=x, y=y, colour=factor(cluster))) +
geom_polygon(data=eldata, aes(x=x, y=y, colour=factor(cluster), group=cluster), alpha=0.1)}
