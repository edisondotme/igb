run <- function() {
  # main.R
  
  setwd("igb/lai-r-proj/")
  
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
  
  # 877 xval beginning of circle
  # 3021 xval end of circle
  # radius of circle is 1072
  # approximate center is 1949
  center <- c(median(1:nr), median(1:nc))
  center <- c(1965, 1418)
  r <- 1400
  r <- 1072.5
  r <- 1000
  # assign all the z values outside the radius r
  im$z[sqrt((im$rows - center[1])^2 + (im$cols - center[2])^2) > r] = -1
  # im <- imager::as.cimg(im)
  im <- imager::as.cimg(obj = im$z, x = max(im$rows), y = max(im$cols))
  image(1:nr, 1:nc, matrix(im$z, nrow=nr, byrow=FALSE), col=gray((0:32)/32) )
  return(threshold(im))
}

crop <- function(file = "data/test_photos/004_1_Oct3_2015_GOPRO002.JPG") {
  source("./R/th_minimum.R")
  source("./R/bimodtest.R")
  source("./R/convolution.R")
  im <- imager::load.image(file = file)
  im <- imager::B(im)
  nr <- dim(im)[1]
  nc <- dim(im)[2]
  im <- as.data.frame(im)
  im$z <- rep(1, length(im))
  im$cc <- im$z
  
  center <- c(1965, 1418) # hard coded, add dynamic adustability for this value
  r <- 1000 # also hard coded, add adjustability
  
  # assign all the values outside the circle to placeholder value
  im$value[sqrt((im$x - center[1])^2 + (im$y - center[2])^2) > r] = NA # note: need to be -1 for plotting a cimg object
  # im <- imager::as.cimg(obj = im, dims = c(nr, nc, 1, 1))
  # image(1:nr, 1:nc, matrix(im$value, nrow=nr, byrow=FALSE), col=gray((0:32)/32) ) # plotting
  return(imager::as.cimg(im))
}

cropcam <- function() {
  im <- imager::load.image("data/cameraman.png")
  im <- imager::B(im)
  nr <- dim(im)[1]
  nc <- dim(im)[2]
  im <- as.data.frame(im)
  im$z <- rep(1, length(im))
  im$cc <- im$z
  
  # center <- c(1965, 1418) # hard coded, add dynamic adustability for this value
  # r <- 1000 # also hard coded, add adjustability
  
  center <- c(nr/2, nc/2)
  r <- nr/4
  
  # assign all the values outside the circle to placeholder value
  im$value[sqrt((im$x - center[1])^2 + (im$y - center[2])^2) > r] = NA # note: need to be -1 for plotting a cimg object
  # im <- imager::as.cimg(obj = im, dims = c(nr, nc, 1, 1))
  # image(1:nr, 1:nc, matrix(im$value, nrow=nr, byrow=FALSE), col=gray((0:32)/32) ) # plotting
  return(im)
}

prep <- function() {
  im <- imager::load.image('data/test_photos/004_1_Oct3_2015_GOPRO002.JPG')
}