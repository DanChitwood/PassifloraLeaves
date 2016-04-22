#Read in ggplot2 and reshape2 packages

library(ggplot2)
library(reshape2)

#Read in data

data <- read.table("./0.SD_values_by_trait.txt", header=TRUE)

#Melt data to visualize

mdata <- melt(data, id=c(c("trait","order","rev_order","order_trait","rev_order_trait")))
head(mdata)

#Plot out data

p <- ggplot(mdata, aes(x=variable, y=rev_order_trait, fill=value))
p + geom_tile() + scale_fill_gradient2(low="black", mid="orange", high="yellow", midpoint=1) 
