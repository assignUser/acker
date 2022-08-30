file <- system.file("data/default_acker.shp", package = "acker")
# if(file == ""){
#   print(getwd())
#   file <- file.path(getwd(),"default_acker/default_acker.shp")
#   print(file)
# }
default_acker <- terra::vect(file)
