file <- system.file("data/drought_index.tif", package = "acker")
# if(file == ""){
#   print(getwd())
#   print(list.files("..", recursive = T))
#   file <- "drought_index.tif"
# }
  
drought_index <- terra::rast(file)
