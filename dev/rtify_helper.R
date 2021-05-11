library(dplyr)
library(tidyr)
library(magick)
library(ggforce)

loadImageResize <- function(filepath, longest_dim = 100) {
  stopifnot(is.numeric(longest_dim))
  
  img <- image_read(filepath)
  
  # Get dimensions
  img_w <- image_info(img)$width
  img_h <- image_info(img)$height
  img_ratio <- img_w / img_h
  
  # Resize the longest dimension to the longest_dim pixels
  if (img_w >= img_h) {
    img <- image_resize(img, longest_dim)
  } else {
    img <- image_resize(img, (paste0("x", longest_dim)))
  }
  list(image = img, image_ratio = img_ratio)
}

img2df <- function(img) {
  
  # Create array and number rows and columns
  img_array <- drop(as.integer(img[[1]]))
  rownames(img_array) <- 1:nrow(img_array)
  colnames(img_array) <- 1:ncol(img_array)
  
  # Create data frame from array and rename columns
  as.data.frame.table(img_array) %>% 
    `colnames<-`(c("y", "x", "b")) %>% 
    mutate(
      across(everything(), as.numeric),
      # convert b (0-255) to bf (1-0), so that "brighter" values become smaller points
      bf = 1 - b / 255
    )
}

portraitLine <- function(img,
                         col_fill = "black",
                         col_bg = "#E335C2") {

  img_df <- img %>% 
    image_convert(colorspace = "gray") %>% 
    img2df() %>% 
    # Create extra "steps" for the sine curves
    rowwise() %>% 
    mutate(t = list(x + seq(0, 1, by = 0.05))) %>% 
    unnest(t)
  
  ggplot(img_df) +
    geom_path(aes(x = t, y = y + bf * sin(4 * pi * t) / 2, group = y), color = col_fill) +
    scale_y_reverse() +
    coord_fixed(expand = FALSE) + 
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = col_bg, color = NA))
  
}

portraitSplitbar <- function(img,
                             col_fill = "black",
                             col_bg = "#F1E34C"
) {
  img_df <- img %>% 
    image_convert(colorspace = "gray") %>% 
    img2df()
  
  ggplot(img_df) +
    geom_rect(aes(xmin = x, xmax = x + bf * 0.9, ymin = y, ymax = y + 0.85), fill = col_fill, color = NA) +
    scale_y_reverse() +
    coord_fixed(expand = FALSE) +
    theme_void() +
    theme(legend.position = "none", plot.background = element_rect(fill = col_bg, color = NA)) 
}

portraitBspline <- function(img,
                            image_ratio,
                            col_fill = "black",
                            col_bg = "#F1E34C"
) {
  img_df <- img %>% 
    image_convert(colorspace = "gray") %>% 
    img2df() %>%
    mutate(n = row_number()) %>%
    group_by(n) %>%
    mutate(
      bx = list(c(x, x + bf * runif(1, 1, 3), x + bf * runif(1, 1, 3), x)),
      by = list(c(y + bf * runif(1, 1, 3), y + bf * runif(1, 1, 3), y, y))
    ) %>%
    ungroup() %>%
    unnest(c(bx, by))
    
    ggplot(img_df) +
    geom_bspline_closed(aes(x = bx, y = by, group = n, alpha = bf), 
                        fill = col_fill, color = NA, size = 0.3) +
    scale_y_reverse() +
    scale_alpha_identity() +
    # coord_fixed(expand = FALSE) +
    theme_void() +
    theme(legend.position = "none",
          aspect.ratio = image_ratio,
          plot.background = element_rect(fill = col_bg, color = NA))
  
}

# from portraits points
# https://github.com/gkaramanis/aRtist/tree/main/portraits/portraits%20points
portraitPoint <- function(img, 
                          shape = 16,
                          col_fill = "black", 
                          col_bg = "#F1E34C") {

  img_df <- img %>% 
    image_convert(colorspace = "gray") %>% 
    img2df() 
    
  
  ggplot(img_df) +
    geom_point(aes(x = x, y = y, size = bf), 
               color = col_fill,
               shape = shape) +
    scale_y_reverse() +
    scale_size_continuous(range = c(0, 3)) +
    coord_fixed(expand = FALSE) +
    theme_void() +
    theme(legend.position = "none", 
          plot.background = element_rect(fill = col_bg, color = NA))
  
}

portaitAscii <- function(img) {
  
  ramp <- " .:-=+*#%@"
  
  img <- img %>% 
    image_convert(colorspace = "gray")
  
  # Create array and number rows and columns
  img_array <- drop(as.integer(img[[1]]))
  rownames(img_array) <- 1:nrow(img_array)
  colnames(img_array) <- 1:ncol(img_array)
  
  # Create data frame from array and rename columns
  img_df <- as.data.frame.table(img_array) %>% 
    `colnames<-`(c("y", "x", "b")) %>% 
    mutate(
      across(everything(), as.numeric),
      # map b (0-255) to bf (1-0), so that "brighter" values become smaller numbers
      bf = 1 - b / 255,
      # and then map to i (1-10) to use with the character ramp
      i = round(bf * 10),
      # replace 0 with 1
      i = if_else(i == 0, 1, i)
    ) %>%
    rowwise() %>%
    mutate(c = substr(ramp, i, i)) %>% 
    arrange(y, x)
  
  # Plot
  ggplot(img_df) +
    # size of text is calculated from image height and is approximate
    geom_text(aes(x, y, label = c), family = "Courier Bold", size = max(img_df$y)/20) +
    scale_y_reverse() +
    coord_fixed() +
    theme_void() 
   
  # # Find width of output
  # w <- max(img_df$x)
  # # Keep odd lines to prevent stretching
  # txt_df <- img_df %>%
  #   filter(y %% 2 == 1)
}

portraitRgb <- function(img, image_ratio) {
  
  # Get channels
  r <- image_channel(img, channel = "red")
  g <- image_channel(img, channel = "green")
  b <- image_channel(img, channel = "blue")
  
  # Here I should make a function or loop, but how?
  img_array_r <- drop(as.integer(r[[1]]))
  rownames(img_array_r) <- 1:nrow(img_array_r)
  colnames(img_array_r) <- 1:ncol(img_array_r)
  
  img_array_b <- drop(as.integer(b[[1]]))
  rownames(img_array_b) <- 1:nrow(img_array_b)
  colnames(img_array_b) <- 1:ncol(img_array_b)
  
  img_array_g <- drop(as.integer(g[[1]]))
  rownames(img_array_g) <- 1:nrow(img_array_g)
  colnames(img_array_g) <- 1:ncol(img_array_g)
  
  # Make data frame for each channel
  img_df_r <- as.data.frame.table(img_array_r) %>% 
    mutate(Var3 = "red")
  
  img_df_g <- as.data.frame.table(img_array_g) %>% 
    mutate(Var3 = "green")
  
  img_df_b <- as.data.frame.table(img_array_b) %>% 
    mutate(Var3 = "blue")
  
  # rbind all channel data frames
  img_df <- rbind(img_df_r, img_df_g, img_df_b) %>% 
    `colnames<-`(c("y", "x", "c", "v")) %>% 
    distinct(x, y, c, v) %>% 
    mutate(
      across(c(x, y, c), as.numeric),
      
      # map v (0-255) to vf (0-1), in order to use for alpha
      vf = c / 255,
      # offset for "pixels"
      x = case_when(
        v == "green" ~ x + 0.3,
        v == "blue" ~ x + 0.6,
        TRUE ~ x
      )
    )
  
  ggplot(img_df) +
    geom_rect(aes(xmin = x, xmax = x + 0.3, ymin = y, ymax = y + 0.9, fill = v, alpha = vf), color = NA) +
    scale_y_reverse() +
    scale_fill_identity() +
    scale_alpha_identity() +
    # Set expand to TRUE for "frame" around plot
    coord_cartesian(expand = FALSE) +
    theme_void() +
    theme(legend.position = "none",
          aspect.ratio = 1/image_ratio,
          plot.background = element_rect(fill = "black", color = NA))
  
}