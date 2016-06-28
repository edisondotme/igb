bimodtest <- function(y, debug = FALSE) {
  # test if a histogram is bimodal
  
  # in: 
  # y histogram counts
  
  # out
  # true if histogram is bimodal, otherwise false
  
  # this script is ported from the original matlab code by Antti Niemistro
  # GNU General Public License
  
  
  # y <- y$counts
  len <- length(y)
  if (debug) {print(len)}
  b <- FALSE
  modes <- 0
  
  # count the number of modes of the histogram in a loop.
  # If the number exceeds 2, return with boolean value false.
  
  for (k in 2:(len - 1)) {
    if (debug) {print(c(k, modes))}
    if (y[k - 1] < y[k] & y[k + 1] < y[k]) {
      modes <- modes + 1
      if (modes > 2) {
        break
      }
    }
  }
 if (debug) {print(modes)} 
  if (modes == 2) {
    b <- TRUE
  }
  if (modes > 2) {
    if (debug) {print('number of modes is greater than 2')}
    # b <- TRUE
  }
  return(b)
}
# consider using the r package diptest instead of porting this yourself.
# that package might be better

init <- function() {
  library(imager)
  file_path <- "C:\\Users\\me\\Documents\\igb\\cameraman.png"
  I <- load.image(file = file_path)
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
