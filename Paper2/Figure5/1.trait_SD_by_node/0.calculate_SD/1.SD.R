#Read in data

data <- read.table("./0.both_land_efd.txt", header=TRUE)

#Isolate just the traits

just_traits <- data[c(10:119)]

#Scale the traits

scaled_data <- scale(just_traits)

#Add back heteroblastic node data to the scaled traits

scaled_data_het <- cbind(data[8], scaled_data)

#Create a table to keep standard deviation data from the loop

overall.table <- matrix(nrow=110, ncol=10)

#for loop for each of i traits, calculate the standard deviation across each of j heteroblastic nodes

for(i in 2:111) {

print(i)

trait <- scaled_data_het[c(1,i)]

for(j in 1:10) {

print(j)

sub <- subset(trait, heteroblasty==j)

overall.table[i-1,j] <- sd(sub[,2])

}}

#Add column names to standard deviation data

colnames(overall.table) <- c("node1", "node2", "node3", "node4", "node5", "node6", "node7", "node8", "node9", "node10")

#Add row names to standard deviation data

rownames(overall.table) <- colnames(data[10:119])