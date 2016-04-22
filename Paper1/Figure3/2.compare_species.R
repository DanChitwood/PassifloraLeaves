#Important note: The following code is written using Momocs version 0.2-6. The current version of Momocs is 1.0 (http://vbonhomme.github.io/Momocsdoc/).

#Read in Momocs (v 0.2-6)
library(Momocs)

#Convert the .nef file from SHAPE (http://lbm.ab.a.u-tokyo.ac.jp/~iwata/shape/) to a COE object using the NEF2COE function. This function was written by Ryan Felice, as provided via Vincent Bonhomme.

NEF2COE<-function (nef.path){
  nef <- readLines(nef.path)
  HARMO.l <- grep(pattern = "HARMO", nef)
  nb.h <- as.numeric(substring(nef[HARMO.l], 8))
  nef <- nef[-(1:HARMO.l)]
  nb.coo <- length(nef)/(nb.h + 1)
  coo.i <- 1:nb.coo
  coo.beg <- (coo.i - 1) * (nb.h + 1) + 1
  coo.end <- coo.beg + nb.h
  res <- matrix(NA, nrow = nb.coo, ncol = nb.h * 4,
      dimnames = list(nef[coo.beg], paste(rep(LETTERS[1:4], each = nb.h),
      1:nb.h, sep = "")))
  for (i in seq(along = coo.i)) {
    nef.i <- nef[(coo.beg[i]+1):coo.end[i]]
    x <- as.numeric(unlist(strsplit(nef.i, " ")))
    x1<-x[!is.na(x)]
    a.i<-x1[seq(1,length(x1),4)]
    b.i<-x1[seq(2,length(x1),4)]
    c.i<-x1[seq(3,length(x1),4)]
    d.i<-x1[seq(4,length(x1),4)]
    res[i, ]<-c(a.i,b.i,c.i,d.i)
  }
  return(Coe(res,method="eFourier"))}

#Create a COE object from the .nef file

pass <- NEF2COE("./0.passiflora_nef.txt")

#Read in species identities

species <- read.table("./1.just_species.txt", header=TRUE)

#Assign species identities to the @fac slot in the pass COE object

pass@fac <- species

#Calculate mean shapes

mean_species <- meanShapes(pass)

#Visualize the mean shape for a species by leveraging the tps.iso function comparing species against themselves. Colors used in the figure are taken from colorbrewer2.org

tps.iso(mean_species$mucronata, mean_species$mucronata, cont=FALSE, iso.levels=0, shp.col=c("#a6761d"), shp.border=c("black"), shp.lwd=c(5,5))

