convolution <-
  function(u,
           v = rep(1 / 3, 3),
           same = FALSE,
           debug = FALSE) {
    # http://www.mathworks.com/help/matlab/ref/conv.html#budgyuq-3
    # perform a convolution in 1D
    # In
    # u: a 1D array 
    # v: a 1D array
    # same: boolean, if True, returns the central part of the convolution
    # the same length as u
    # Out: w - the convolution
    # --------------------------------------------------------#
    
    m <- length(u)
    n <- length(v)
    wLength <- m + n - 1
    w <- c()
    
    # Get the whole convolution
    # Note to self: I wonder if this can be optimized by only calculating the center
    # if same is True
    # this can probably be turned into an apply statement as well
    for (k in 1:wLength) {
      j <- seq(from = max(1, k + 1 - n),
               by = 1,
               to = min(k, m))
      w[k] <- sum(u[j] * v[k - j + 1])
    }
    
    # Get central part of w same length as u
    if (m %% 2 == wLength %% 2 && same) {
      # conditions for perfect center
      selection <-
        w[(((wLength - m) / 2) + 1):((wLength - m) / 2 + (m))]
      return(selection)
    } else if (m %% 2 != wLength %% 2 && same) {
      # conditions for off center
      w <- w[wLength + 1] <- NA
      selection <- w[(((wLength - m) / 2) + 1):((wLength - m) / 2 + m)]
      return(selection[1:wLength])
    } else {
      # conditions for same == FALSE
      # return the full convolution
      return(w)
    }
  }