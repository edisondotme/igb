init <- function() {
    library(imager)
    image <- imager::load.image("C:\\Users\\me\\Documents\\igb\\cameraman.png")
    image <- B(image)
    image_histogram <- hist(x = image*255, breaks = 0:255)
    return(image)
}
