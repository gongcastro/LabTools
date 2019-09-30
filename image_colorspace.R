# rbg_image: Tranform image to RGB colour space
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

image_colorspace <-
  function(
    
    input.path    = getwd(),
    output.path   = getwd(),
    input.format  = 'jpg',
    output.format = 'jpg',
    colorspace    = 'rgb'
    
  ){
    
    # load packages
    require(magrittr)
    require(magick)
    
    # locate files
    input.filenames  <- list.files(input.path, full.names = FALSE, pattern = input.format) 
    input.paths      <- list.files(input.path, full.names = TRUE, pattern = input.format)
    output.filenames <- paste0(input.filenames)
    output.paths     <- paste0(output.path, "/", output.filenames)
    n                <- length(input.paths)
    
    # rotate images
    for (i in 1:n){
      image_read(input.paths[i]) %>%
        image_quantize(., max = 1000, colorspace = colorspace) %>%
        image_write(output.paths[i], format = output.format)
      message(paste0(i, "/", n, " images transformed to RGB colorspace"))
    }  
  }
