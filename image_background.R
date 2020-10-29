# image_background: add a background to the image
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

##########################################################

image_background <-
  function(
    
    input.path  = getwd(),   # input folder
    output.path = getwd(),   # output folder
    color  = '#808080', # in HEX code
    height = 500,       # height in pixels
    width  = 500        # width in pixels

  ){
    
    # load packages
    require(magrittr) # for using pipes
    require(magick)   # for manipulating images
    
    # locate files
    input.filenames  <- list.files(input.path, full.names = FALSE) 
    input.path       <- list.files(input.path, full.names = TRUE)
    output.filenames <- input.filenames
    output.path      <- paste0(output.path, '/', output.filenames)
    n                <- length(input.path)
    
    # create background
    bg <- image_blank(width = width, height = height, color = color)
    
    # rotate images
    for (i in 1:n){
      image_read(input.path[i]) %>%
        image_composite(
          bg, .,
          operator = 'Overlay',            # put image over background
          offset = geometry_point(50, 50)  # in the middle
        ) %>%
        image_write(
          output.path[i],
          format = 'png')
      message(paste0(i, '/', n, ' backgrounds set'))
    }  
  }
