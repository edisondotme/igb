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
    source('./middle_algorithm.R')

    if (debug) {print('debug mode enabled')}

    # Load images
    image <- load.image(file = I)
    image <- B(image) # only blue band
    image_histogram <- hist(x = image*255,
                            breaks = 0:n,
                            col = 'blue',
                            main = 'Histogram')
    image_histogram <- image_histogram$counts
    # difference here ^^^
    if (debug) {print(head(image_histogram, 20))}

    iter <- 0
    if (debug) {readline('starting iterations')}
    if (debug) {readline(bimodtest(image_histogram))}

    while (bimodtest(image_histogram)) {
        # if (debug) {readline(iter)}

        h <- rep(1/3, 3)
        # image_histogram <- stats::convolve(x = image_histogram, y = h, type = "open")
        image_histogram <- conv(u = image_histogram)
        iter <- iter + 1
        # If the histogram turns out not to be bimodal, set T to zero
        if (iter > 10000) {
            T <- 0
            if (debug) {readline('histogram not bimodal; T is zero')}
            return(T)
        }
    }
    if (debug) {print(iter)}

    #
    peakfound <- FALSE
    
    for (k in 2:n) {
        if (debug) {readline(k)}

        if ( image_histogram[k-1] < image_histogram[k] & image_histogram[k + 1] < image_histogram[k]) {
            # peak found
            peakfound <- TRUE
            if (debug) {readline('peakfound')}
        }
        if (peakfound & image_histogram[k-1] >= image_histogram[k+1] & image_histogram[k+1] >= image_histogram[k] ) {
            T <- k-1
            return(T)
        }
    }
}

