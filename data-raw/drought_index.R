get_drought_data <- function(min_year = 1995, max_year = 2021) {
  urls <- glue::glue(
    "https://opendata.dwd.de/climate_environment/CDC/",
    "grids_germany/annual/drought_index/",
    "grids_germany_annual_drought_index_{min_year:max_year}17.asc.gz"
  )
  drought_rasters <- purrr::map(
    urls,
    ~ terra::rast(
      glue::glue("/vsigzip//vsicurl/{.x}")
    )
  )

  # drought_rasters <- glue::glue("drought_index_{min_year:max_year}") %>%
  #   purrr::walk2(drought_rasters, function(name, rast) {
  #     names(rast) <- name
  #   })
  drought_raster <- do.call(c, drought_rasters)
  terra::crs(drought_raster) <- "EPSG:31467"
  terra::project(drought_raster, "EPSG:4326")
}

drought_index <- get_drought_data()
# SpatRaster can not be saved as rda due to GDAL involvement.
writeRaster(drought_index, "inst/drought_index.tiff", filetype = "GTiff", overwrite = TRUE)
#usethis::use_data(drought_index, overwrite = TRUE)
