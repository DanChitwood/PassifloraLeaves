#Read in MASS, reshape2, and ggplot2 packages

library(MASS)
library(reshape2)
library(ggplot2)

#Read in Procrustes coordinate data

data <- read.table("./0.reformated_Proc_coord.txt", header=TRUE)

#LDA performed by species

#lda_spe <- lda(species~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+x7+y7+x8+y8+x9+y9+x10+y10+x11+y11+x12+y12+x13+y13+x14+y14+x15+y15, data=data, CV=TRUE)

#First attempted LDA using all coordinates. But because of colinearity, only half of the leaf is used

lda_spe <- lda(species~x1+y1+x2+y2+x3+y3+x7+y7+x8+y8+x9+y9+x10+y10+x11+y11, data=data, CV=TRUE)

#Create a dataframe with actual and predicted species

actual_spe <- as.data.frame(data$species)
predicted_spe <- as.data.frame(lda_spe$class)
spe_predict <- cbind(actual_spe, predicted_spe)
colnames(spe_predict) <- c("actual_spe", "predicted_spe")

#Create a table with actual vs. predicted species

t_spe_predict <- table(spe_predict)

#Transform the table into proportions and use the melt function to reformat into a plottable format

p_spe_predict <- prop.table(t_spe_predict, 1)
m_spe_predict <- melt(p_spe_predict, id="actual_spe")

m_spe_predict$actual_spe <- factor(m_spe_predict$actual_spe, levels=c("coriacea","misera","biflora","capsularis","micropetala","organensis","pohlii","rubra","tricuspis","caerulea","cincinnata","edmundoi","gibertii","hatschbachii","kermesina","mollissima","setacea","suberosa","tenuifila","amethystina","foetida","gracilis","morifolia","actinia","miersii","sidifolia","triloba","alata","edulis","ligularis","nitida","racemosa","villosa","coccinea","cristalina","galbana","malacophylla","maliformis","miniata","mucronata"))

m_spe_predict$predicted_spe <- factor(m_spe_predict$predicted_spe, levels=c("coriacea","misera","biflora","capsularis","micropetala","organensis","pohlii","rubra","tricuspis","caerulea","cincinnata","edmundoi","gibertii","hatschbachii","kermesina","mollissima","setacea","suberosa","tenuifila","amethystina","foetida","gracilis","morifolia","actinia","miersii","sidifolia","triloba","alata","edulis","ligularis","nitida","racemosa","villosa","coccinea","cristalina","galbana","malacophylla","maliformis","miniata","mucronata"))

#Plot out a confusion matrix

p <- ggplot(m_spe_predict, aes(predicted_spe, actual_spe, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + coord_fixed()

#LDA performed by heteroblasty

#Read in Procrustes coordinate data

data <- read.table("./0.reformated_Proc_coord.txt", header=TRUE)

#Only look at first 10 nodes, as almost all plants have at least this many leaves

sub10 <- subset(data, heteroblasty<11)
sub10$heteroblasty <- as.factor(sub10$heteroblasty)

#lda_hetero <- lda(heteroblasty~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+x7+y7+x8+y8+x9+y9+x10+y10+x11+y11+x12+y12+x13+y13+x14+y14+x15+y15, data=sub10, CV=TRUE)

#First attempted LDA using all coordinates. But because of colinearity, only half of the leaf is used

lda_hetero <- lda(heteroblasty~x1+y1+x2+y2+x3+y3+x7+y7+x8+y8+x9+y9+x10+y10+x11+y11, data=sub10, CV=TRUE)

#Create a dataframe with actual and predicted heteroblastic node

actual_het <- as.data.frame(sub10$heteroblasty)
predicted_het <- as.data.frame(lda_hetero$class)
hetero_predict <- cbind(actual_het, predicted_het)
colnames(hetero_predict) <- c("actual_het", "predicted_het")

#Create a table with actual vs. predicted species

t_hetero_predict <- table(hetero_predict)

#Transform the table into proportions and use the melt function to reformat into a plottable format

p_hetero_predict <- prop.table(t_hetero_predict, 1)
m_hetero_predict <- melt(p_hetero_predict, id="actual_het")

#Plot out a confusion matrix

p <- ggplot(m_hetero_predict, aes(predicted_het, actual_het, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + theme_bw()+ coord_fixed()
