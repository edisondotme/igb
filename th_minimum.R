T <- function(I, n = 255) {
    # Find a global threshold for a grayscale image by choosing the threshold to
    # be in the valley of the bimodal histogram. The method is also known as the
    # mode method.
    
    # In:
    # I : Grayscale image
    # n : maximum graylevel (default 255)
    
    
    # Copyright (C) 2004-2013 Antti Niemist
    # This file is part of HistThresh toolbox

    iter <- 0
    while (bimodtest(y)) {
        # do thing
        h <- matrix(1, 3) / 3
        y <- convolution(y, h, 'same')
        iter += 1

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
    if ( y(k - 1) < y(k) & y(k + 1) < y(k)) {
        #peak found
        peakfound <- TRUE
    }
    if (peakfound & y(k-1) >= y(k) & y(k + 1) >= y(k) ) {
        T <- k - 1
        return()
    }
}
