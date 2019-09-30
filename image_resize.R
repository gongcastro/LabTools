# resize_image: rescale images
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

##########################################################

image_resize <-
  function(
    
    input.path    = getwd(),
    output.path   = getwd(),
    input.format  = 'png',
    output.format = 'png',
    size          = '400x400'
    
  ){
    
    # load packages
    require(magrittr)
    require(magick)
    
    # locate files
    input.filenames  <- list.files({{input.path}}, full.names = FALSE) 
    input.paths      <- list.files({{input.path}}, full.names = TRUE)
    output.filenames <- paste0(input.filenames)
    output.paths     <- paste0({{output.path}}, '/', output.filenames)
    n                <- length(input.paths)
    
    # resize images
    for (i in 1:n){
      image_read(input.paths[i]) %>%
        magick::image_resize(., {{size}}) %>%
        image_write(., path = output.paths[i], format = {{output.format}})
      message(paste0(i, '/', n, ' images resized'))
    }  
  }
