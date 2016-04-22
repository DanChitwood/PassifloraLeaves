#Read in ggplot2 and grid, for the arrows

library(ggplot2)
library(grid)

#Read in data

data <- read.table("./0.just_arrow_ends.txt", header=TRUE)

#Visualize tsne with arrows

p <- ggplot(data, aes(x=V1, y=V2, xend=endV1, yend=endV2, color=class))

p + geom_segment(arrow=arrow(length=unit(0.5, "cm"))) + geom_point(aes(x=V1, y=V2), shape=1, colour="black", size=2.5) + scale_colour_brewer(type="qual", palette=2)+ theme_bw() + xlim(c(-35,40)) + ylim(c(-35, 35))
