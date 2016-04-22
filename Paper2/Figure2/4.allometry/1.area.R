#Read in ggplot2

library(ggplot2)

#Read in data

data <- read.table("./0.reformated_Proc_coord.txt", header=TRUE)

#I normally don't use attach, but will use here for ease because of the complexity of the calculations

attach(data)

#Let's first calculate all the different areas of the leaf using the shoelace algorithm

#V1 = proximal vein
#V2 = midvein
#A1 = proximal area
#A2 = distal area

data$V1 <- abs(x1*(y7-y2) + x7*(y2-y1) + x2*(y1-y7))/2

data$V2 <- abs(x1*(y2-y6) + x2*(y3-y1) + x3*(y11-y2) + x11*(y4-y3) + x4*(y5-y11) + x5*(y6-y4) + x6*(y1-y5))/2

data$A1 <- abs(x2*(y7-y3) + x7*(y8-y2) + x8*(y9-y7) + x9*(y3-y8) + x3*(y2-y9))/2

data$A2 <- abs(x3*(y9-y11) + x9*(y10-y3) + x10*(y11-y9) + x11*(y3-y10))/2

data$total <- data$V1 + data$V2 + data$A1 + data$A2

#Now, let's detach the data

detach(data)

#Below, is the color scheme used in the figure, derived from colorbrewer2.org

#V1=#a6611a Dark brown (proximal vein)
#V2=#dfc27d Light brown (midvein)
#A1=#018571 Dark green (proximal area)
#A2=#80cdc1 Light green (distal area)

#For all leaves, graph out each area (V1, V2, A1, A2) as the square root of the sub-region area plotted against the total leaf area

p <- ggplot(data, aes(x=sqrt(total), y=sqrt(V1)))
p + geom_point(colour="#a6611a", alpha=0.25, size=2) + geom_point(aes(x=sqrt(total), y=sqrt(V2)), colour="#dfc27d", alpha=0.25, size=2)  + geom_point(aes(x=sqrt(total), y=sqrt(A1)), colour="#018571", alpha=0.25, size=2)  + geom_point(aes(x=sqrt(total), y=sqrt(A2)), colour="#80cdc1", alpha=0.25, size=2) + theme_bw()

