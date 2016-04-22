#Read in ggplot2 and reshape2 packages

library(ggplot2)
library(reshape2)

#Read in species LDA results by node

data <- read.table("./0.species_by_node_LDA.txt", header=TRUE)

#Melt the data to reformat for visualization

mdata <- melt(data, id=c("class","species"))

#Plot out the data

p <- ggplot(mdata, aes(x=variable, y=species, fill=value))
p + geom_tile()+ scale_y_discrete(limits=rev(levels(mdata$species))) + scale_fill_gradient2(low="black", mid="orange", high="yellow", midpoint=0.5)





























































###########################
####LDA BY HETEROBLASTY####
###########################

data <- read.table("./2.harmonics_info.txt", header=TRUE)

names(data)
head(data)
summary(data$species)

#!!!!!! Removed NAs
data <- subset(data, species != "NA")

sub10 <- subset(data, heteroblasty<11)
sub10$heteroblasty <- as.factor(sub10$heteroblasty)

lda_hetero <- lda(heteroblasty~x1+y1+x2+y2+x3+y3+x7+y7+x8+y8+x9+y9+x10+y10+x11+y11+    A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20, data=sub10, CV=TRUE)

actual_het <- as.data.frame(sub10$heteroblasty)
predicted_het <- as.data.frame(lda_hetero$class)

hetero_predict <- cbind(actual_het, predicted_het)
colnames(hetero_predict) <- c("actual_het", "predicted_het")

t_hetero_predict <- table(hetero_predict)

#write.table(t_hetero_predict, "4.predicted_hetero.txt")

p_hetero_predict <- prop.table(t_hetero_predict, 1)
m_hetero_predict <- melt(p_hetero_predict, id="actual_het")

head(m_hetero_predict)

p <- ggplot(m_hetero_predict, aes(predicted_het, actual_het, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + theme_bw()

cor.test(as.numeric(hetero_predict$actual_het), as.numeric(hetero_predict$predicted_het), method="spearman")






































































































#What is the percent separation by each LD?
lda_spe$svd/sum(lda_spe$svd)

#Let's predict the species each leaf belongs to using the LDA results

spe_table <- t(table(predict(lda_spe, type="class")$class, known_spe$species))

spe_table_df <- as.data.frame(spe_table)

expanded_spe_table <- spe_table_df[rep(row.names(spe_table_df), spe_table_df$Freq), 1:2]

#^_^_^_^_^_ species confusion matrix here
#________________________________________

p <- ggplot(expanded_spe_table, aes(Var2, Var1))
p + geom_point(position="jitter", alpha=0.3, size=2) + theme_bw() + theme(axis.text.x=element_text(angle=90, hjust=1)) + geom_vline(x=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5), alpha=0.2) +geom_hline(y=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5), alpha=0.2) 

#Write out a table with LDA values and associated covariates. Note: posterior probabilities of samples belonging to a given species are given

spe.lda.values <- predict(lda_spe, known_spe[10:51])

spe.lda.val.info <- cbind(known_spe[,1:9],spe.lda.values)

#Let's visualize LD1/LD2 values by species

p <- ggplot(spe.lda.val.info, aes(x=x.LD1, y=x.LD2, colour=species))
p + geom_point()

#That's a little messy. So let's look at just the morphologically distinct species, like we did before for the morphospace

Aacout <- subset(spe.lda.val.info, species=="Ampelopsis_aconitifolia")
Abrevi <- subset(spe.lda.val.info, species=="Ampelopsis_brevipedunculata")
Vlab <- subset(spe.lda.val.info, species=="Vitis_labrusca")
Vrip <- subset(spe.lda.val.info, species=="Vitis_riparia")
Vrup <- subset(spe.lda.val.info, species=="Vitis_rupestris")
Vthu <- subset(spe.lda.val.info, species=="Vitis_thunbergii")

select_species <- rbind(Aacout,Abrevi,Vlab,Vrip,Vrup,Vthu)

p <- ggplot(select_species, aes(x=x.LD1, y=x.LD2, colour=species))
p + geom_point(alpha=0.6) + scale_colour_brewer(type="qual", palette=2) + theme_bw() + stat_ellipse(size=2, level=0.95)

p <- ggplot(select_species, aes(x=x.LD1, y=x.LD2, colour=factor(leaf_stage)))
p + geom_point(alpha=0.6) + scale_colour_brewer(type="div", palette=2) + theme_bw() + stat_ellipse(size=2, level=0.95)

p <- ggplot(select_species, aes(x=x.LD1, y=x.LD2, colour=factor(leaf_num)))
p + geom_point(alpha=0.6) + scale_colour_brewer(type="div", palette=2) + theme_bw() + stat_ellipse(size=2, level=0.95)

?stat_ellipse


################
#LDA by leaf_stage
################

#Let's train an LDA on the first 10 plastochrons (developmental stage) but then use this information to predict the hypothesis that the first 10 mature leaves (leaf number) are developmentally delayed

#Let's first perform an LDA for leaf_stage on all those leaves < 11th poisition in the shoot
p <- ggplot(data, aes(x=factor(leaf_stage)))
p + geom_histogram()

sub10 <- subset(data, leaf_stage<11)

lda_stage <- lda(factor(leaf_stage)~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+x7+y7+x8+y8+x9+y9+x10+y10+x11+y11+x12+y12+x13+y13+x14+y14+x15+y15+x16+y16+x17+y17+x17+y17+x18+y18+x19+y19+x20+y20+x21+y21, data=sub10)

#What is the percent separation by each LD?
lda_stage$svd/sum(lda_stage$svd)

#Let's now predict position of leaves in the shoot based on the resulting discriminants and visualize

stage_table <- t(table(predict(lda_stage, type="class")$class, sub10$leaf_stage))

stage_table_df <- as.data.frame(stage_table)

expanded_stage_table <- stage_table_df[rep(row.names(stage_table_df), stage_table_df$Freq), 1:2]

#^_^_^_^_^_ leaf_stage confusion matrix here
#________________________________________

p <- ggplot(expanded_stage_table, aes(Var2, Var1))
p + geom_point(position="jitter", alpha=0.3) + theme_bw() + theme(axis.text.x=element_text(angle=90, hjust=1)) + geom_vline(x=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5), alpha=0.2) +geom_hline(y=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5), alpha=0.2) + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())

#^_^_^_^_^_ leaf_stage correlation here
#________________________________________

cor.test(x=as.numeric(expanded_stage_table$Var1), y=as.numeric(expanded_stage_table$Var2), method="spearman")

#Let's write out a table with predcited leaves by stage and posterior probability estimates and make a graph of LD values by leaf_stage

stage.lda.values <- data.frame(predict(lda_stage, sub10[10:51]))

stage.lda.values.info <- cbind(sub10, stage.lda.values)

write.table(stage.lda.values.info, "lda_stage.txt")

p <- ggplot(stage.lda.values.info, aes(x=x.LD1, y=x.LD2, colour=factor(leaf_stage)))
p + geom_point() + scale_colour_brewer(type="div", palette=3) + stat_ellipse(size=2) + theme_bw()


p <- ggplot(stage.lda.values.info, aes(x=x.LD1, y=x.LD2, colour=factor(species)))
p + geom_point() + scale_colour_brewer(type="qual", palette=3) + stat_ellipse(size=2) + theme_bw()


p <- ggplot(stage.lda.values.info, aes(x=x.LD1, y=x.LD2, colour=factor(leaf_num)))
p + geom_point() + scale_colour_brewer(type="div", palette=1) + stat_ellipse(size=2) + theme_bw()


#Finally, let's train an LDA on the first 10 leaf stages and predict the leaf stage of the first 10 leaf numbers

train <- subset(data, species!="Vitis_sp")
train10 <- subset(train, leaf_stage<11)
train10$species <- factor(train10$species)

test <- subset(data, species!="Vitis_sp")
test10 <- subset(test, leaf_num<11)
test10$species <- factor(test10$species)

lda_train <- lda(leaf_stage~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+x7+y7+x8+y8+x9+y9+x10+y10+x11+y11+x12+y12+x13+y13+x14+y14+x15+y15+x16+y16+x17+y17+x18+y18+x19+y19+x20+y20+x21+y21, data=train10)

predicted <- data.frame(predict(lda_train, test10)$class)

predicted_with_info <- cbind(predicted, test10)

colnames(predicted_with_info)[1] <- "predicted_leaf_stage_for_num"

p <- ggplot(predicted_with_info, aes(x=factor(leaf_num), y=factor(predicted_leaf_stage_for_num)))
p + geom_point(position="jitter", alpha=0.5) + theme_bw()



################
#LDA by leaf_number
################


#Let's first perform an LDA for leaf_num on all those leaves < 11th poisition in the shoot
p <- ggplot(data, aes(x=factor(leaf_num)))
p + geom_histogram()

sub10 <- subset(data, leaf_num<11)

lda_num <- lda(factor(leaf_num)~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+x7+y7+x8+y8+x9+y9+x10+y10+x11+y11+x12+y12+x13+y13+x14+y14+x15+y15+x16+y16+x17+y17+x17+y17+x18+y18+x19+y19+x20+y20+x21+y21, data=sub10)

#What is the percent separation by each LD?
lda_num$svd/sum(lda_num$svd)

#Let's now predict position of leaves in the shoot based on the resulting discriminants and visualize

num_table <- t(table(predict(lda_num, type="class")$class, sub10$leaf_num))

num_table_df <- as.data.frame(num_table)

expanded_num_table <- num_table_df[rep(row.names(num_table_df), num_table_df$Freq), 1:2]

#^_^_^_^_^_ leaf_num confusion matrix here
#________________________________________

p <- ggplot(expanded_num_table, aes(Var2, Var1))
p + geom_point(position="jitter", alpha=0.3) + theme_bw() + theme(axis.text.x=element_text(angle=90, hjust=1)) + geom_vline(x=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5), alpha=0.2) +geom_hline(y=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5), alpha=0.2) + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())

#^_^_^_^_^_ leaf_stage correlation here
#________________________________________

cor.test(x=as.numeric(expanded_num_table$Var1), y=as.numeric(expanded_num_table$Var2), method="spearman")

#Let's write out a table with predcited leaves by stage and posterior probability estimates and make a graph of LD values by leaf_stage

num.lda.values <- data.frame(predict(lda_num, sub10[10:51]))

num.lda.values.info <- cbind(sub10, num.lda.values)

write.table(num.lda.values.info, "lda_number.txt")

p <- ggplot(num.lda.values.info, aes(x=x.LD1, y=x.LD2, colour=factor(leaf_num)))
p + geom_point() + scale_colour_brewer(type="div", palette=1) + stat_ellipse(size=2, level=0.95) + theme_bw()


p <- ggplot(num.lda.values.info, aes(x=x.LD1, y=x.LD2, colour=factor(leaf_stage)))
p + geom_point() + scale_colour_brewer(type="div", palette=2) + stat_ellipse(size=2) + theme_bw()


p <- ggplot(num.lda.values.info, aes(x=x.LD1, y=x.LD2, colour=factor(species)))
p + geom_point() + scale_colour_brewer(type="qual", palette=2) + stat_ellipse(size=2) + theme_bw()






