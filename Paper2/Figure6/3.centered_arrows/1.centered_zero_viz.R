#Read in ggplot2 and grid, for the arrows

library(ggplot2)
library(grid)

#Read in the data

data <- read.table("./0.centered_zero.txt", header=TRUE)

#Produce graph with centered arrows

p <- ggplot(data, aes(x=zeroV1start, y=zeroV2start, xend=zeroV1end, yend=zeroV2end, color=class))

p + geom_segment(arrow=arrow(length=unit(0.5, "cm"))) + geom_point(aes(x=0, y=0), shape=1, colour="black", size=2.5) + scale_colour_brewer(type="qual", palette=2)+ theme_bw() + xlim(c(-35,40)) + ylim(c(-35, 35))
