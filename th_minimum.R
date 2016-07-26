threshold <- function(I = "C:\\Users\\me\\Documents\\igb\\cameraman.png", n = 255, debug = FALSE) {
    # Find a global threshold for a grayscale image by choosing the threshold
    # to be in the valley of the bimodal histogram. The method is also known
    # as the mode method.
    # In:
    # I : Grayscale image filepath
    # n: maximum graylevel (default 255)

    #---------------------------------------------------------------------#

    library(stats)
    library(imager)
    source('./bimodtest.R')
    source('./convolution.R')

    if (debug) {print('debug mode enabled')}

    # Load images
    image <- load.image(file = I)
    image <- B(image) # only blue band
    image_histogram <- hist(x = image*255,
                            breaks = 0:n,
                            plot = debug)
    image_histogram <- image_histogram$counts
    if (debug) {print(head(image_histogram, 20))}

    iter <- 0
    if (debug) {readline('starting iterations')}
    if (debug) {readline(bimodtest(image_histogram))}

    while (!bimodtest(image_histogram)) {
        # if (debug) {readline(iter)}

        # image_histogram <- stats::convolve(x = image_histogram, y = h, type = "open")
        image_histogram <- convolution(u = image_histogram, same = TRUE)
        iter <- iter + 1
        if (debug) {
          plot(image_histogram, type = 'l', col = 'red')
          readline('pause to look at graph')
        }
        # If the histogram turns out not to be bimodal, set T to zero
        if (iter > 10000) {
            T <- 0
            if (debug) {readline('histogram not bimodal; T is zero')}
            return(T)
        }
    }
    if (debug) {print(iter)}
    # return(image_histogram)

    #
    peakfound <- FALSE
    
    for (k in 2:n) {
      if ( image_histogram[k-1] < image_histogram[k] & image_histogram[k + 1] < image_histogram[k]) {
          peakfound <- TRUE
          }
      if (peakfound & image_histogram[k-1] >= image_histogram[k+1] & image_histogram[k+1] >= image_histogram[k] ) {
        T <- k-1
        return(T)
      }
    }
}
