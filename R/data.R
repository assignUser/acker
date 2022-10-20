
#' DWD Climate Data Center (CDC): Annual grids of drought index (de Martonne) over Germany, version v1.0
#'
"drought_index"


#' Data from years with min max and median DI in Fermany
#'
#' year drought_index
#' 2002      51.79878
#' 2005      41.32579
#' 2018      28.30215
#' @seealso [acker::drought_index]
"min_di_weather"


.get_acker_vect <- function(...) {
  ids <- list(...) %>%
    unlist() %>%
    paste(collapse = ",")
  data <- try({
    osmdata::opq_osm_id(id = ids, type = "way") %>%
      osmdata::osmdata_sf()
  })

  if (inherits(data, "try-error")) {
    stop("Data retrival from Overpass failed.")
  }

  vec <- terra::vect(data$osm_polygons)
  terra::aggregate(vec)
}
get_acker_vect <- memoise::memoise(.get_acker_vect)

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
  rdwd::dataDWD(link, dir = tempfile(), quiet = TRUE) %>% mutate(lat = lat, lon = lon)
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

.clean_weather_data <- function(data) {
  impute <- function(values) {
    avg <- mean(values, na.rm = TRUE)
    ifelse(is.na(values), avg, values)
  }

  # Precompute ap_a/b and etr to reduce srad calc time by ~90%
  xy <- matrix(c(data$lon[[1]], data$lat[[1]]), ncol = 2, nrow = 1)
  ap_a <- get_ap_a(xy)
  ap_b <- get_ap_b(xy)

  data %>%
    transmute(
      date = lubridate::as_date(MESS_DATUM),
      year = lubridate::year(date),
      month = lubridate::month(date),
      doy = lubridate::yday(date),
      etr = sirad::extrat(i = 1:366, lat = sirad::radians(xy[, 2]))$ExtraTerrestrialSolarRadiationDaily %>% `[`(doy),
      rain_mm = RSK,
      srad = sirad::ap(date,
        lat = xy[, 2], lon = xy[, 1],
        extraT = etr,
        A = ap_a, B = ap_b,
        SSD = SDK
      ),
      etr = NULL,
      t_min = TNK,
      t_max = TXK,
      average_temp = TMK
    ) %>%
    group_by(year, month) %>%
    mutate(across(where(is.numeric), impute)) %>%
    dplyr::as_tibble()
}
clean_weather_data <- memoise::memoise(.clean_weather_data)

