find_middle <- function(vec) {
  median(vec) %>%
    {
      abs(vec - .)
    } %>%
    {
      min(.) == .
    } %>%
    which() %>%
    head(1) %>%
    {
      vec == vec[.]
    }
}

# freq 0.007 for finer resolution
# freq 0.05 for coarser res
acker_noise <- function(akr, freq = 0.007, r = c(-1, 1)) {
  ambient::noise_perlin(dim(akr)[1:2], frequency = freq, fractal = "none") %>% ambient::normalise(to = r)
}

get_acker_center <- function(ak) {
  stopifnot(terra::is.polygons(ak))
  terra::centroids(ak) %>% terra::crds()
}

get_acker_drought_index <- function(ak) {
  indices <- terra::extract(drought_index, get_acker_center(ak)) %>%
    t() %>%
    data.frame() %>%
    dplyr::rename(di = ".") %>%
    transmute(
      year = rownames(.) %>% gsub(".*_(\\d{4})17.*", "\\1", .),
      drought_index = di
    )
  rownames(indices) <- NULL
  indices
}

get_di_selection <- function(weather, di) {
  di %>%
    filter(year %in% weather$year) %>%
    mutate(
      di = round(drought_index, 2),
      drought_index = NULL,
      min = di == min(di),
      max = di == max(di),
      middle = find_middle(di)
    ) %>%
    filter(min | max | middle)
}

.get_drought_data <- function(min_year = 1995, max_year = 2021) {
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

  drought_raster <- do.call(c, drought_rasters)
  terra::crs(drought_raster) <- "EPSG:31467"
  terra::project(drought_raster, "EPSG:4326")
}
get_drought_data <- memoise::memoise(.get_drought_data)


get_ap_a <- function(xy) {
  sirad:::APa_map %>%
    terra::rast() %>%
    terra::extract(xy) %>%
    unlist()
}

get_ap_b <- function(xy) {
  sirad:::APb_map %>%
    terra::rast() %>%
    terra::extract(xy) %>%
    unlist()
}

ensure_data <- function(names, env = parent.frame()) {
  exist <- purrr::map_lgl(names, exists, envir = env)
  if (any(!exist)) {
    data(list = names[!exist], package = "acker", envir = env)
  }
}
