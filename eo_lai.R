# eo_lai.R

library(raster)
# dependency: sp

library(tcltk)

#############

setwd(tk_choose.dir())

photos <- list.files()

# single raster layer
p1 <- raster::raster(photos[1])

# raster stack with multiple layers. see: str(mystack)
mystack <- raster::stack(photos[1])

# mystack[[3]] is the third layer in the stack and usually the blue band

