#Load ggplot2

library(ggplot2)

#Read in data

data <- read.table("./0.ordered_variables.txt", header=TRUE)

#Make heatmap

t <- ggplot(data=data, aes(x=order_var1, y=order_var2, fill=-log( BH.NA + (2.2*10^(-16)) ,10)))

t + geom_tile() + scale_fill_gradient(low="blue", high="yellow", na.value="gray98")  +theme_bw() + theme(axis.text.x=element_text(angle=-90)) + coord_fixed()

