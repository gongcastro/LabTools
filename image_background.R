# image_background: add a background to the image
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

##########################################################

image_background <-
  function(
    
    input  = getwd(),   # input folder
    output = getwd(),   # output folder
    color  = '#808080', # in HEX code
    height = 500,       # height in pixels
    width  = 500        # width in pixels

  ){
    
    # load packages
    require(magrittr) # for using pipes
    require(magick)   # for manipulating images
    
    # locate files
    input.filenames  <- list.files(input, full.names = FALSE) 
    input.paths      <- list.files(input, full.names = TRUE)
    output.filenames <- input.filenames
    output.paths     <- paste0(output, '/', output.filenames)
    n                <- length(input.paths)
    
    # create background
    bg <- image_blank(width = width, height = height, color = color)
    
    # rotate images
    for (i in 1:n){
      image_read(input.paths[i]) %>%
        image_composite(
          bg, .,
          operator = 'Overlay',            # put image over background
          offset = geometry_point(50, 50)  # in the middle
        ) %>%
        image_write(
          output.paths[i],
          format = 'png')
      message(paste0(i, '/', n, ' backgrounds set'))
    }  
  }
