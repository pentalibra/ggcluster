# Test functionality using testthat library
# 
# Author: Andrie
###############################################################################





hc <- hclust(dist(USArrests), "ave")
dhc <- as.dendrogram(hc)
#get_cluster_data(dhc, type="rectangle")$segments
#get_data_dendrogram(dhc, type="rectangle")$text

#get_data_dendrogram(dhc, type="triangle")
# ggplot(dhcdata) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1)) + coord_flip()
# dhcdata <- get_data_dendrogram(dhc, type="triangle")
# ggplot(dhcdata) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1))


data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
#get_data_tree(cpus.ltr)
#' tree_label_data <- get_data_tree_labels(cpus.ltr)
#' leaf_label_data <- get_data_tree_leaf_labels(cpus.ltr)

#hc <- hclust(dist(USArrests), "ave")
#dhcdata <- get_cluster_data(hc, type="rectangle")
#dhclabels <- get_cluster_labels(hc, type="rectangle")
#ggplot() + 
#		geom_segment(data=dhcdata, aes(x=x0, y=y0, xend=x1, yend=y1)) +
#		geom_text(data=dhclabels, aes(x=x, y=y, label=text), size=3, hjust=0) +
#		coord_flip() + scale_y_reverse(expand=c(0.2, 0))




###############################################################################

context("dendrogram")

test_that("get_data_dendrogram() returns a data frame", {
			
			expect_that(get_cluster_data(dhc, type="rectangle"), 
					is_a("data.frame"))

			expect_that(get_cluster_labels(dhc, type="rectangle"), 
					is_a("data.frame"))
			
			ddd <- get_cluster_data(dhc, type="rectangle")
			expect_that(nrow(ddd), equals(196))
			
			ddd <- get_cluster_data(dhc, type="triangle")
			expect_that(nrow(ddd), equals(98))
			
		})

context("hclust")

test_that("get_data_hclust() returns a data frame", {
			
			expect_that(get_cluster_data(hc, type="rectangle"), 
					is_a("data.frame"))
			
			ddd <- get_cluster_data(hc, type="rectangle")
			expect_that(nrow(ddd), equals(196))
			
			ddd <- get_cluster_data(hc, type="triangle")
			expect_that(nrow(ddd), equals(98))
			
		})


context("tree")

test_that("get_data_tree() returns a data frame", {
			
			expect_that(get_cluster_data(cpus.ltr), 
					is_a("data.frame"))
			
			expect_that(get_cluster_labels(cpus.ltr), 
					is_a("data.frame"))
			
			expect_that(get_data_tree_leaf_labels(cpus.ltr), 
					is_a("data.frame"))
			
		})


context("clusterplots and dendrograms")

test_that("plots work ok", {
			
		hc <- hclust(dist(USArrests), "ave")
		dhcdata <- get_cluster_data(hc, type="rectangle")
		dhclabels <- get_cluster_labels(hc, type="rectangle")
		p <- ggplot() + 
				geom_segment(data=dhcdata, aes(x=x0, y=y0, xend=x1, yend=y1)) +
				geom_text(data=dhclabels, aes(x=x, y=y, label=text)) +
				coord_flip() + scale_y_reverse(expand=c(0.2, 0))
		expect_that(p, is_a("ggplot"))
		print(p)
	 
		})


