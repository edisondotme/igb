# Take convolution of two vectors

conv <- function(u, v = rep(1/3, 3), type = 'open', debug = F) {
    # find convolution in 1D
    # input
    # u: vector
    # v: vector
    # 

    # library(stats)
    # find convolution
    # length(u)+length(v)-1
    convolution <- round(stats::convolve(x = u, y = v, type = type), digits = 1)
    l <- length(conv)
    if(debug) {print(l)}

    # take the last element and make it first
    convolution <- replace(convolution, 1, tail(convolution, 1))
    # delete last element in vector
    convolution <- head(convolution, -1)
    return(convolution)
}
