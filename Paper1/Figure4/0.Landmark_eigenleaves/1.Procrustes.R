#Read in the 'shapes' library

library(shapes)

#Specify landmark number (k), landmark dimensions (m), and number of samples (n

k <- 15
m <- 2
n <- 3319

#Read in the data with the associated parameters

data <- read.in("./0.just_landmarks.txt",k,m)

#Perform the Procrustes analysis

GPA <- procGPA(data, reflect=FALSE)

#How much variance does each PC represent?

GPA$percent

#Let's look at the eigenleaves using the shapepca function and add lines using the joinline argument

shapepca(GPA, pcno=c(1,2,3), joinline=c(1,7,8,9,10,11,12,13,14,15,6,1,7,2,3,9,3,11,4,13,4,5,15), mag=0.5)

shapepca(GPA, pcno=c(4,5,6), joinline=c(1,7,8,9,10,11,12,13,14,15,6,1,7,2,3,9,3,11,4,13,4,5,15), mag=0.5)
