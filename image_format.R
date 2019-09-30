# image_format: change image format
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

##########################################################

image_format <-
  function(
    
    input.path  = getwd(), # input folder
    output.path = getwd(), # output folder
    to          = 'png'    # output format
    
  ){
    
    # load packages
    require(magrittr) # for using pipes
    require(magick)   # for manipulating images
    
    # locate files
    input.filenames  <- list.files(input.path, full.names = FALSE) 
    output.filenames <- paste0(input.filenames)
    input.paths      <- list.files(input.path, full.names = TRUE)
    output.paths     <- paste0(output.path, "/", output.filenames)
    n                <- length(input.paths)
    
    # rotate images
    for (i in 1:n){
      image_read(input.paths[i]) %>%
        image_write(
          output.paths[i],
          format = to)
      message(paste0(i, "/", n, " images reformated"))
    }  
  }
