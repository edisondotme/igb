T <- function(I, n = 255) {
  # Find a global threshold for a grayscale image by choosing the threshold to
  # be in the valley of the bimodal histogram. The method is also known as the
  # mode method.
  # In:
  # I : Grayscale image
  # n : maximum graylevel (default 255)
  # Copyright (C) 2004-2013 Antti Niemist
  # This file is part of HistThresh toolbox
  
  image <- load.image(file = I)
  image <- B(image)  # get only blue band
  image_histogram <- hist(x = image, breaks = n, col = 'blue', main = cat('Histogram of ', I))
  
  iter <- 0
  while (bimodtest(image_histogram)) {
    # do thing
    h <- matrix(1, 3) / 3
    image_histogram <- convolution(image_histogram, h, 'same')
    iter <- iter + 1
    # if the histogram turns out not to be bimodal, set T to zero
    if (iter > 10000) {
      T <- 0
      return()
    }
  }
}

peakfound <- FALSE

for (k in 2:n) {
    # do a thing
    if ( image_histogram[k - 1] < image_histogram[k] & image_histogram[k + 1] < image_histogram[k]) {
        #peak found
        peakfound <- TRUE
    }
    if (peakfound & image_histogram[k-1] >= image_histogram[k] & image_histogram[k + 1] >= image_histogram[k] ) {
        T <- k - 1
        return()
    }
}
