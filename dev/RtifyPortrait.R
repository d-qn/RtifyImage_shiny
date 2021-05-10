source("rtify_helper.R")
  
#filepath <- "images/duc.png"
filepath <- "images/keanu.jpg"

img <- loadImageResize(filepath, longest_dim = 80)

img[[1]] %>% 
  portaitAscii()

img[[1]] %>% 
  portraitPoint (shape = 17)

img[[1]] %>% 
  portraitLine()

img[[1]] %>% 
  portraitRgb(image_ratio = img[[2]])

img[[1]] %>% 
  portraitSplitbar()

img[[1]] %>% 
  portraitBspline(image_ratio = img[[2]])