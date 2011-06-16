\name{ellipsoid_data}
\alias{ellipsoid_data}
<<<<<<< HEAD
\title{Calculates ellipsoid data for use in plot_cluster...}
\usage{ellipsoid_data(data, x="x", y="y", cluster="cluster",
    ellipse_quantile=0.67)}
\description{Calculates ellipsoid data for use in plot_cluster}
\keyword{internal}
\arguments{\item{data}{}
\item{x}{}
\item{y}{}
\item{cluster}{}
\item{ellipse_quantile}{}}
=======
\title{Creates ellipsoid from dataframe containing x and y...}
\usage{ellipsoid_data(data, x="x", y="y", cluster="cluster")}
\description{Creates ellipsoid from dataframe containing x and y}
\details{Generic function to extract cluster labels from a model into a data frame}
\seealso{\code{\link{cluster_data}}, \code{\link{plot_cluster}}}
\arguments{\item{data}{A data frame containing at least three columns: x, y and cluster.
x and y represent the plotting coordinates, and cluster is the cluster allocation.}
\item{x}{Character string, indicating the x coordinate}
\item{y}{Character string, indicating the y coordinate}
\item{cluster}{Character string, indicating the cluster allocation}}
\examples{data(iris)
d <- iris[, -5]
model <- kmeans(d, 3)
cdata <- cluster_data(model)
pc <- princomp(d)
pcdata <- cluster_data(pc)
eedata <- cbind(cdata, pcdata)
eldata <- ellipsoid_data(eedata)

ggplot() + 
geom_point(data=cbind(cdata, pcdata), 
aes(x=x, y=y, colour=factor(cluster))) +
geom_polygon(data=eldata, 
aes(x=x, y=y, colour=factor(cluster), group=cluster), 
alpha=0.1)}
>>>>>>> 8fa87a461f33ab2fb3deee548d5d82666c94bb35
