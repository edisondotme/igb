bimodtest <- function(y, debug = FALSE) {
  # test if a histogram is bimodal
  
  # In:
  # y: histogram counts
  
  # Out
  # true if histogram is bimodal, otherwise false
  
  # this script is ported from the original matlab code by Antti Niemistro
  # GNU General Public License
  # ------------------------------------------------------ #
  
  len <- length(y)
  b <- FALSE
  modes <- 0
  
  # count the number of modes of the histogram in a loop.
  # If the number exceeds 2, return with boolean value false.
  
  # turn the following into an apply statement for speed
  for (k in 2:(len - 1)) {
    if (y[k - 1] < y[k] & y[k + 1] < y[k]) {
      modes <- modes + 1
      if (modes > 2) {
        break
      }
    }
  }
  if (modes == 2) {
    b <- TRUE
  }
  if (modes > 2) {
  }
  return(b)
}
