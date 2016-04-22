#Read in ggplot2

library(ggplot2)

#Read in Procrustes coordinates

data <- read.table("./0.reformated_Proc_coord.txt", header=TRUE)

#Specify the first heteroblastic node to compare, subset leaves from that node, take the mean leaf, transpose and convert into dataframe to visualize

hetero <- 1

sub <- subset(data, heteroblasty==hetero)
mean <- colMeans(sub[6:35])
mean <- as.data.frame(t(mean))

#Specify the second heteroblastic node to compare, subset leaves from that node, take the mean leaf, transpose and convert into dataframe to visualize

hetero2 <- 10

sub2 <- subset(data, heteroblasty==hetero2)
mean2 <- colMeans(sub2[6:35])
mean2 <- as.data.frame(t(mean2))

#Specify attributes of the first averaged leaf

size=2
alpha=1
colour="#7fbc41"

#Specify attributes of the second averaged leaf

m_size=2
m_alpha=1
m_colour="#c51b7d"

#Plot out the first leaf

p <- ggplot(data=mean, aes(x=x1, y=y1, xend=x7, yend=y7))
p + geom_segment(size=size, alpha=alpha, colour=colour) + 
geom_segment(data=mean, aes(x=x7, y=y7, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x3, y=y3, xend=x11, yend=y11), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x11, y=y11, xend=x4, yend=y4), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x4, y=y4, xend=x5, yend=y5), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x5, y=y5, xend=x15, yend=y15), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x15, y=y15, xend=x6, yend=y6), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x6, y=y6, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x7, y=y7, xend=x8, yend=y8), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x8, y=y8, xend=x9, yend=y9), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x9, y=y9, xend=x10, yend=y10), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x10, y=y10, xend=x11, yend=y11), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x11, y=y11, xend=x12, yend=y12), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x12, y=y12, xend=x13, yend=y13), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x13, y=y13, xend=x14, yend=y14), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x14, y=y14, xend=x15, yend=y15), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x3, y=y3, xend=x9, yend=y9), size=size, alpha=alpha, colour=colour) +
geom_segment(data=mean, aes(x=x4, y=y4, xend=x13, yend=y13), size=size, alpha=alpha, colour=colour) +

#Plot out the second leaf

geom_segment(data=mean2, aes(x=x1, y=y1, xend=x7, yend=y7), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x7, y=y7, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x3, y=y3, xend=x11, yend=y11), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x11, y=y11, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x4, y=y4, xend=x5, yend=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x5, y=y5, xend=x15, yend=y15), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x15, y=y15, xend=x6, yend=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x6, y=y6, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x7, y=y7, xend=x8, yend=y8), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x8, y=y8, xend=x9, yend=y9), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x9, y=y9, xend=x10, yend=y10), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x10, y=y10, xend=x11, yend=y11), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x11, y=y11, xend=x12, yend=y12), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x12, y=y12, xend=x13, yend=y13), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x13, y=y13, xend=x14, yend=y14), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x14, y=y14, xend=x15, yend=y15), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x3, y=y3, xend=x9, yend=y9), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean2, aes(x=x4, y=y4, xend=x13, yend=y13), size=m_size, alpha=m_alpha, colour=m_colour) +

#Procrustes leaves are inverted, so uninvert

scale_y_reverse() + 

#Make sure the axes are equal

coord_fixed() +

#Remove all the other graph stuff

theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())
      