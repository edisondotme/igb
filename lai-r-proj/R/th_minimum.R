threshold <-
  function(I = NA,
           n = 255,
           debug = FALSE) {
    # Find a global threshold for a grayscale image by choosing the threshold
    # to be in the valley of the bimodal histogram. The method is also known
    # as the mode method.
    
    # In:
    # I : Grayscale image filepath OR a cimg object with 1 color channel
    # n: maximum graylevel (default 255)
    
    # Out:
    # T: Threshold value
    
    #---------------------------------------------------------------------#
    
    # library(stats) # Do I actually need this package?
    library(imager)
    # these assume the r script is run in the correct directory
    source('R/bimodtest.R')
    source('R/convolution.R')
    
    # this if statement needs to be fixed
    if (class(I)[1] == 'cimg') {
      message('cimg type detected. Proceeding')
      # image <- B(I) # only blue band for reasons
      image <- I
      image_histogram <- hist(x = image * n,
                              breaks = 0:n,
                              plot = debug)
      image_histogram <- image_histogram$counts
    } else if (is.na(I)) { # possible bug condition where first element is NA but input stil given
      stop("Must have input for I")
    } else if (is.vector(I)) {
      message('Vector detected. Using vector mode.')
      # Note: this is repetitive code, this should be cleaned up
      image_histogram <- hist(x = I * n,
           breaks = 0:n,
           plot = debug)
      image_histogram <- image_histogram$counts
    } else {
      message('Image file detected. Proceeding')
      # Load and preprocess image and make histogram
      image <- load.image(file = I)
      image <- B(image) # only blue band for reasons
      image_histogram <- hist(x = image * n,
                              breaks = 0:n,
                              plot = debug)
      image_histogram <- image_histogram$counts
    }
    
    iter <- 0
    
    # while histogram is not bimodal,
    # convolve it until it is
    while (!bimodtest(image_histogram)) {
      image_histogram <-
        convolution(u = image_histogram, same = TRUE)
      iter <- iter + 1
      
      # delete this debug statement after showing to Andrew maybe?
      if (debug) {
        plot(image_histogram, type = 'l', col = 'red')
        readline('pause to look at graph')
      }
      # If the histogram turns out not to be bimodal, set T to zero and return
      if (iter > 10000) {
        T <- 0
        return(T)
      }
    }
    
    peakfound <- FALSE
    
    # if the peak is found, return the threshold value
    for (k in 2:n) {
      # readline(cat(k, image_histogram[k]))
      status <- peakfound &
        image_histogram[k - 1] >= image_histogram[k + 1] & # scope access error: element 256. Please fix
        image_histogram[k + 1] >= image_histogram[k]
      message(status, k)
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
