#Read in libraries

library(ggplot2)
library(ape)
library(ggdendro)

#Read in data
data <- read.table("./0.scaled_metrics.txt",header=TRUE)

n.data <- data[,c(1:110)]

#Generate hierarchical clustering

hc <- hclust(as.dist(1-abs(cor(n.data, method="spearman", use="pairwise.complete.obs"))), method="ward")

#Plot out hierarhical clustering

plot(as.phylo(hc), type="cladogram", cex=0.5, label.offset=0.01)
