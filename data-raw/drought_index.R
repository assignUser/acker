drought_index <- get_drought_data()
# SpatRaster can not be saved as rda due to GDAL involvement.
terra::writeRaster(drought_index,
  "data/drought_index.tif",
  filetype = "GTiff",
  gdal = c("COMPRESS=ZSTD", "TFW=YES"),
  overwrite = TRUE
)
