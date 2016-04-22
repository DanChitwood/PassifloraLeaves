#Read in MASS and ggplot2 packages

library(MASS)
library(ggplot2)

#Read in Procrustes coordinate and EFD data

data <- read.table("./0.both_land_efd.txt", header=TRUE)

#LDA performed by species

#Specify which node to perform the LDA by species with

node = 4

#Subset just those leaves from the node

sub <- subset(data, heteroblasty==node)

#Perform the LDA

lda_spe <- lda(species~x1+y1+x2+y2+x3+y3+x7+y7+x8+y8+x9+y9+x10+y10+x11+y11+  A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20, data=sub, CV=TRUE)

#Create a dataframe with actual and predicted species

actual_spe <- as.data.frame(sub$species)
predicted_spe <- as.data.frame(lda_spe$class)
spe_predict <- cbind(actual_spe, predicted_spe)
colnames(spe_predict) <- c("actual_spe", "predicted_spe")

#Create a table with actual vs. predicted species

t_spe_predict <- table(spe_predict)


