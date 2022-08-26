#' TODO document data sets / make it load on attach
#'
#"drought_index"

# TODO Document
#' data from years with min max and median DI in germany
#'
#' year drought_index
#' 2002      51.79878
#' 2005      41.32579
#' 2018      28.30215
"min_data"


# 729667622 729394749 729394750

get_acker_vect <- function(...) {
  sf <- get_acker_sf(...)
  terra::vect(sf)
}



.get_acker_sf <- function(...) {
  ids <- list(...) %>%
    unlist() %>%
    paste(collapse = ",")

  osmdata::opq_osm_id(id = ids, type = "way") %>%
    osmdata::osmdata_sf() %>%
    {
      sf::st_union(.$osm_polygons)
    }
}
get_acker_sf <- memoise::memoise(.get_acker_sf)

get_acker_center <- function(ak) {
  stopifnot(terra::is.polygons(ak))
  terra::centroids(ak) %>% terra::crds()
}

get_acker_drought_index <- function(ak) {
  indices <- extract(drought_index, get_acker_center(ak)) %>%
    t() %>%
    data.frame() %>%
    rename(di = ".") %>%
    transmute(
      year = rownames(.) %>% gsub(".*_(\\d{4})17.*", "\\1", .),
      drought_index = di
    )
  rownames(indices) <- NULL
  indices
}

get_acker_weather <- function(ak) {
  ak_xy <- get_acker_center(ak)
  weather <- get_station_data(ak_xy) %>% clean_weather_data()
}

.get_station_data <- function(lat, lon,
                              radius = 50,
                              min_date = as.Date("2020-12-31")) {
  if (length(lat) == 2) {
    lon <- lat[1]
    lat <- lat[2]
  }
  found <- FALSE
  while (!found && !is.na(min_date)) {
    if (radius >= 200) min_date <- NA
    id <- try(get_id(lat, lon, radius, min_date), silent = TRUE)
    found <- is.numeric(id)
    if (!found) {
      radius <- radius + 10
    }
  }

  if (inherits(id, "try-error")) {
    stop("No station found matching the criteria!")
  }
  # TODO use fallback data

  link <- rdwd::selectDWD(id = id, res = "daily", var = "kl", per = "h")
  rdwd::dataDWD(link, dir = tempfile(), quiet = TRUE)
}
# TODO cache on disk
get_station_data <- memoise::memoise(.get_station_data)

.get_id <- function(lat, lon, radius = 50, min_date = as.Date("2020-12-31")) {
  suppressWarnings(
    station <- rdwd::nearbyStations(
      lat, lon, radius,
      res = "daily",
      var = "kl",
      per = "h",
      mindate = min_date,
      quiet = TRUE
    )
  )
  na.omit(station)$Stations_id[[1]]
}
get_id <- memoise::memoise(.get_id)

clean_weather_data <- function(data) {
  impute <- function(values) {
    avg <- mean(values, na.rm = TRUE)
    ifelse(is.na(values), avg, values)
  }

  data %>%
    transmute(
      date = lubridate::as_date(MESS_DATUM),
      year = lubridate::year(date),
      doy = lubridate::yday(date),
      rain_mm = RSK,
      sun = SDK,
      t_min = TNK,
      t_max = TXK,
      average_temp = TMK
    ) %>%
    group_by(year, lubridate::month(date)) %>%
    mutate(across(where(is.numeric), impute)) %>%
    dplyr::as_tibble()
}

get_di_selection <- function(weather, di) {
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
  di %>%
    filter(year %in% weather$year) %>%
    mutate(
      min = drought_index == min(drought_index),
      max = drought_index == max(drought_index),
      middle = find_middle(drought_index)
    ) %>%
    filter(min | max | middle)
}


acker_noise <- function(akr, freq = 0.007, r = c(-1,1)) {
  ambient::noise_perlin(dim(akr)[1:2], frequency = freq, fractal = "none") %>% ambient::normalise(to = r)
}
