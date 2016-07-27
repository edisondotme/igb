convolution <-
  function(u,
           v = rep(1 / 3, 3),
           same = FALSE,
           debug = FALSE) {
    # http://www.mathworks.com/help/matlab/ref/conv.html#budgyuq-3
    m <- length(u)
    n <- length(v)
    wLength <- m + n - 1
    if (debug) {
      print(wLength)
    }
    w <- c()
    
    # find the convolution
    for (k in 1:wLength) {
      j <- seq(from = max(1, k + 1 - n),
               by = 1,
               to = min(k, m))
      w[k] <- sum(u[j] * v[k - j + 1])
      if (debug) {
        print(j)
        print('---')
        print(w[k])
      }
    }
    
    # Get central part of w same length as u
    # note, it always shifts one unit right when wLength is odd
    
    if (m %% 2 == wLength %% 2 && same) {
      # conditions for perfect center
      if (debug) {
        print('perfect center')
      }
      selection <-
        w[(((wLength - m) / 2) + 1):((wLength - m) / 2 + (m))]
      return(selection)
    } else if (m %% 2 != wLength %% 2 && same) {
      # conditions for off center
      if (debug) {
        print('off center')
      }
      w <- w[wLength + 1] <- NA
      selection <- w[(((wLength - m) / 2) + 1):((wLength - m) / 2 + m)]
      return(selection[1:wLength])
    } else {
      # conditions for same == FALSE
      if (debug) {
        print('should only run if same == F')
      }
      return(w)
    }
  }