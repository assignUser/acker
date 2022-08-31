file <- system.file("data/default_acker.shp", package = "acker")

default_acker <- terra::vect(file)
