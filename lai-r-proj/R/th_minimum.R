threshold <-
  function(I = NA,
           n = 255,
           debug = FALSE) {
    # Find a global threshold for a grayscale image by choosing the threshold
    # to be in the valley of the bimodal histogram. The method is also known
    # as the mode method.
    
    # In:
    # I : Grayscale image filepath
    # n: maximum graylevel (default 255)
    
    # Out:
    # T: Threshold value
    
    #---------------------------------------------------------------------#
    
    library(stats)
    library(imager)
    # these assume the r script is run in the correct directory
    # source('./bimodtest.R')
    # source('./convolution.R')
    
    if (is.na(I)) {
      stop("Error: No image path given.")
    }
    
    # Load images
    image <- load.image(file = I)
    image <- B(image) # only blue band for reasons
    image_histogram <- hist(x = image * n,
                            breaks = 0:n,
                            plot = debug)
    
    image_histogram <- image_histogram$counts
    
    iter <- 0
    
    while (!bimodtest(image_histogram)) {
      image_histogram <-
        convolution(u = image_histogram, same = TRUE)
      iter <- iter + 1
      if (debug) {
        plot(image_histogram, type = 'l', col = 'red')
        readline('pause to look at graph')
      }
      # If the histogram turns out not to be bimodal, set T to zero
      if (iter > 10000) {
        T <- 0
        return(T)
      }
    }
    
    peakfound <- FALSE
    
    for (k in 2:n) {
      if (image_histogram[k - 1] < image_histogram[k] &
          image_histogram[k + 1] < image_histogram[k]) {
        peakfound <- TRUE
      }
      if (peakfound &
          image_histogram[k - 1] >= image_histogram[k + 1] &
          image_histogram[k + 1] >= image_histogram[k]) {
        T <- k - 1
        return(T)
      }
    }
  }
