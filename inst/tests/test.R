# Test functionality using testthat library
# 
# Author: Andrie
###############################################################################





#cluster_data(dhc, type="rectangle")$segments
#data_dendrogram(dhc, type="rectangle")$text

#data_dendrogram(dhc, type="triangle")
# ggplot(dhcdata) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1)) + coord_flip()
# dhcdata <- data_dendrogram(dhc, type="triangle")
# ggplot(dhcdata) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1))


### tree
data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
#dtree <- cluster_data(cpus.ltr)
#str(dtree)


### hclust
#hc <- hclust(dist(USArrests), "ave")
#dhcdata <- cluster_data(hc, type="rectangle")
#dhclabels <- cluster_labels(hc, type="rectangle")
#ggplot() + 
#		geom_segment(data=dhcdata, aes(x=x0, y=y0, xend=x1, yend=y1)) +
#		geom_text(data=dhclabels, aes(x=x, y=y, label=text), size=3, hjust=0) +
#		coord_flip() + scale_y_reverse(expand=c(0.2, 0))

### kmeans
data(iris)
iris <- iris[, -5]
#model <- kmeans(d, 3)
#cdata <- cluster_data(model)
#pc <- princomp(d)
#pcdata <- cluster_data(pc)
#eedata <- cbind(cdata, pcdata)
#eldata <- ellipsoid_data(eedata)
#
#ggplot() + 
#		geom_point(data=cbind(cdata, pcdata), aes(x=x, y=y, colour=factor(cluster))) +
#		geom_polygon(data=eldata, aes(x=x, y=y, colour=factor(cluster), group=cluster), alpha=0.1)



###############################################################################

context("dendrogram")

test_that("data_dendrogram() returns a correct classes", {

			hc <- hclust(dist(USArrests), "ave")
			dhc <- as.dendrogram(hc)
			
			expect_that(cluster_data(dhc, type="rectangle"), is_a("list"))

			ddata <- cluster_data(dhc, type="rectangle")
			expect_that(ddata$segments, is_a("data.frame"))
			expect_that(ddata$labels, is_a("data.frame"))
			expect_that(nrow(ddata$segments), equals(196))
			
			ddata <- cluster_data(dhc, type="triangle")
			expect_that(ddata$segments, is_a("data.frame"))
			expect_that(ddata$labels, is_a("data.frame"))
			expect_that(nrow(ddata$segments), equals(98))
			
		})

context("hclust")

test_that("data_hclust() returns the correct classes", {
			
			hc <- hclust(dist(USArrests), "ave")

			expect_that(cluster_data(hc, type="rectangle"), is_a("list"))
			
			ddata <- cluster_data(hc, type="rectangle")
			expect_that(ddata$segments, is_a("data.frame"))
			expect_that(ddata$labels, is_a("data.frame"))
			expect_that(nrow(ddata$segments), equals(196))
			
		})


context("tree")

test_that("data_tree() returns the correct classes", {
			
			expect_that(cluster_data(cpus.ltr), is_a("list"))
			tdata <- cluster_data(cpus.ltr)
			expect_that(tdata$segments, is_a("data.frame"))
			expect_that(tdata$labels, is_a("data.frame"))
			expect_that(tdata$leaf_labels, is_a("data.frame"))
			
		})

context("kmeans")

test_that("kmeans data is extracted correctly", {
	data(iris)
	iris <- iris[, -5]
	model <- kmeans(iris, 3)
	pc <- princomp(iris)
	
	expect_that(cluster_data(model), is_a("data.frame"))
	expect_that(cluster_data(pc), is_a("data.frame"))
	
	
	cdata <- cluster_data(model)
	pcdata <- cluster_data(pc)
	eedata <- cbind(cdata, pcdata)

	expect_that(ellipsoid_data(eedata), is_a("data.frame"))
	
	eldata <- ellipsoid_data(eedata)

	p <- ggplot() + 
			geom_point(data=cbind(cdata, pcdata), aes(x=x, y=y, colour=factor(cluster))) +
			geom_polygon(data=eldata, aes(x=x, y=y, colour=factor(cluster), group=cluster), alpha=0.1)
	expect_that(p, is_a("ggplot"))
	
})



context("clusterplots and dendrograms")

test_that("dendrogram plots", {
			
		hc <- hclust(dist(USArrests), "ave")
		hcdata <- cluster_data(hc, type="rectangle")
		p <- ggplot() + 
				geom_segment(data=hcdata$segments, aes(x=x0, y=y0, xend=x1, yend=y1)) +
				geom_text(data=hcdata$labels, aes(x=x, y=y, label=text)) +
				coord_flip() + scale_y_reverse(expand=c(0.2, 0))
		expect_that(p, is_a("ggplot"))
	 
		})

test_that("plot_cluster works", {
			data(iris)
			iris <- iris[, -5]
			expect_that(plot_cluster(iris, kmeans(iris, 3)), is_a("ggplot"))
			expect_that(plot_cluster(iris, Mclust(iris, 3)), is_a("ggplot"))
			
		})

