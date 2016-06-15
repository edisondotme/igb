bimodtest <- function(y) {
  # test if a histogram is bimodal
  
  # in: 
  # y histogram
  
  # out
  # true if histogram is bimodal, otherwise false
  
  # this script is ported from the original matlab code by Antti Niemistro
  # GNU General Public License
  
  len <- length(y)
  b <- false
  modes = 0
  
  # count the number of modes of the histogram in a loop.
  # If the number exceeds 2, return with boolean value false.
  
  for (k in 2:len - 1) {
    if (y[k - 1] < y[k] & y[k + 1] < y(k)) {
      modes = modes + 1
      if (modes > 2) {
        return()
      }
    }
  }
  
  if (modes == 2) {
    b = TRUE
  }
}
# consider using the r package diptest instead of porting this yourself.
# that package might be better

init <- function() {
  library(imager)
  I <- load.image(file = "C:\\Users\\me\\Downloads\\hemiphoto.JPG")
  I <- B(I)
  I <- as.vector(I)
  I <- I*255
  h <- hist(x = I,
            breaks = 255,
            col = 'blue',
            main = 'Histogram of Blue Band',
            xlab = 'Value',
            ylab = 'Number of pixels')
  
  return(h)
}