# main.R

# setwd("igb/lai-r-proj/")

source("./R/th_minimum.R")
source("./R/bimodtest.R")
source("./R/convolution.R")


# T <- threshold(I = "./data/cameraman.png")

im <- imager::load.image(file = "data/test_photos/004_1_Oct3_2015_GOPRO002.JPG")
im <- imager::B(im)
nr <- dim(im)[1]
nc <- dim(im)[2]
im <- as.data.frame(im)
names(im) = c("rows","cols","z")
center=c(median(1:nr), median(1:nc))
r <- 1400
im$z[sqrt((im$rows - center[1])^2 + (im$cols - center[2])^2) > r] = -1
image(1:nr, 1:nc, matrix(im$z, nrow=nr, byrow=FALSE), col=c("gray80", "black","white"))
