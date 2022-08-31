file <- system.file("data/drought_index.tif", package = "acker")
  
drought_index <- terra::rast(file)
