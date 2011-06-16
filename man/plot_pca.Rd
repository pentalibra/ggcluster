\name{plot_pca}
\alias{plot_pca}
\title{Create principal components analysis plot...}
\usage{plot_pca(data, labeltext=names(data), xLabel="Principal component 1",
    yLabel="Principal component 2", pca_function=princomp,
    labels_at_edge=TRUE, explode=1.1, textsize=3)}
\description{Create principal components analysis plot}
\arguments{\item{data}{The data used for clustering}
\item{labeltext}{The labels to use for each column in the data.  Defaults to names(data).}
\item{xLabel}{x-axis label}
\item{yLabel}{y-axis label}
\item{pca_function}{The function to use for principal component analysis.  This could be \code{\link{princomp}}, \code{\link{prcomp}} or \code{\link{factanal}}.}
\item{labels_at_edge}{If TRUE, labels are printed at at edge of plot, otherwise labels are printed next to each point.}
\item{explode}{A multiplication factor that determines the distance of each label from the point.  If over-plotting of labels is a problem, then use a larger explode factor.}
\item{textsize}{Font size}}
