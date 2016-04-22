#Read in ggplot2
library(ggplot2)

#Read in the Procrustes landmark data
data <- read.table("./0.procrustes_landmarks.txt", header=TRUE)

#Check the names of species available to visualzie
summary(data$species)

#The code below is meant to visualize the 1) mean Procrustes-adjusted shape of the leaf for a species super-imposed with 2) all the Procrustes adjusted shapes of leaves for a species

#Specify the species you wish to visualzie

spe <- "misera"

#Subset the data to create a dataset for only that species

sub <- subset(data, species==spe)

#Take the mean of the subsetted data

mean <- colMeans(sub[6:35])

#Transpose the mean values and create a dataframe

mean <- as.data.frame(t(mean))

#Specify the size, alpha, and colour of the lines for all the leaves

size=1
alpha=0.2
colour="dodgerblue"

#Specify the size, alpha, and colour of the lines for the mean leaf

m_size=2
m_alpha=1
m_colour="black"

#Graph out the mean leaf shape and all the leaves

#This portion of the graph is for all the leaves

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x7, yend=y7))
p + geom_segment(size=size, alpha=alpha, colour=colour) + 
geom_segment(data=sub, aes(x=x7, y=y7, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x3, y=y3, xend=x11, yend=y11), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x11, y=y11, xend=x4, yend=y4), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x4, y=y4, xend=x5, yend=y5), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x5, y=y5, xend=x15, yend=y15), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x15, y=y15, xend=x6, yend=y6), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x6, y=y6, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x7, y=y7, xend=x8, yend=y8), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x8, y=y8, xend=x9, yend=y9), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x9, y=y9, xend=x10, yend=y10), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x10, y=y10, xend=x11, yend=y11), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x11, y=y11, xend=x12, yend=y12), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x12, y=y12, xend=x13, yend=y13), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x13, y=y13, xend=x14, yend=y14), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x14, y=y14, xend=x15, yend=y15), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x3, y=y3, xend=x9, yend=y9), size=size, alpha=alpha, colour=colour) +
geom_segment(data=sub, aes(x=x4, y=y4, xend=x13, yend=y13), size=size, alpha=alpha, colour=colour) +

#This portion of the graph is for the mean leaf

geom_segment(data=mean, aes(x=x1, y=y1, xend=x7, yend=y7), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x7, y=y7, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x3, y=y3, xend=x11, yend=y11), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x11, y=y11, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x4, y=y4, xend=x5, yend=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x5, y=y5, xend=x15, yend=y15), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x15, y=y15, xend=x6, yend=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x6, y=y6, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x7, y=y7, xend=x8, yend=y8), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x8, y=y8, xend=x9, yend=y9), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x9, y=y9, xend=x10, yend=y10), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x10, y=y10, xend=x11, yend=y11), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x11, y=y11, xend=x12, yend=y12), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x12, y=y12, xend=x13, yend=y13), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x13, y=y13, xend=x14, yend=y14), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x14, y=y14, xend=x15, yend=y15), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x3, y=y3, xend=x9, yend=y9), size=m_size, alpha=m_alpha, colour=m_colour) +
geom_segment(data=mean, aes(x=x4, y=y4, xend=x13, yend=y13), size=m_size, alpha=m_alpha, colour=m_colour) +

#The Procrustes coordinates create an inverted leaf, so uninvert the leaf

scale_y_reverse() + 

#Fix the axes scales so the shape is not distorted

coord_fixed() +

#Get rid of all the other graph stuff

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
          

