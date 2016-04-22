#Read in Rtsne package

library(Rtsne)

#Read in scaled trait data

data <- read.table("./0.scaled_rows.txt", header=TRUE)

#set.seed for reproducibility

set.seed(2)

#Perform tsne

#Note that perplexity of 40 chosen as increasing to 50 changed the results little

tsne40 <- Rtsne(as.matrix(data[8:117]), perplexity=40)

#Create dataframe for results

axes40 <- as.data.frame(tsne40$Y)
axes_info40 <- cbind(data[c(1,3,4,5,6)], axes40)







































#####SUB
########

sub <- subset(data, heteroblasty<11)

tsne40 <- Rtsne(as.matrix(sub[8:117]), perplexity=40)

axes40 <- as.data.frame(tsne40$Y)
axes_info40 <- cbind(sub[c(1,3,4,5,6)], axes40)

p <- ggplot(axes_info40, aes(V1, V2, colour=species))
p + geom_point() + ggtitle("Perplexity = 40") 

p <- ggplot(axes_info40, aes(V1, V2, colour=as.factor(heteroblasty)))
p + geom_point() + ggtitle("Perplexity = 40") + scale_colour_manual(values=c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b","#ffffbf","#e6f598","#abdda4","#66c2a5","#3288bd")) + theme_bw()
