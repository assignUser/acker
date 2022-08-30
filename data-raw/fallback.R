# Using the loaded-from-disk data does not work for some
# arcane reason, so get the data fresh from DWD
drought_index <- get_drought_data()
mean_di <- terra::global(drought_index, mean, na.rm = TRUE) %>%
  transmute(
    year = rownames(.) %>% gsub(".*_(\\d{4})17.*", "\\1", .),
    di = mean
  ) %>%
  get_di_selection(data.frame(year = 1995:2021), .)

min_xy <- mean_di %>%
  filter(min) %>%
  glue::glue("grids_germany_annual_drought_index_{data$year}17.asc", data = .) %>%
  terra::subset(drought_index, .) %>%
  terra::where.max(.) %>%
  {
    terra::xyFromCell(drought_index, .[2])
  }

max_xy <- mean_di %>%
  filter(max) %>%
  glue::glue("grids_germany_annual_drought_index_{data$year}17.asc", data = .) %>%
  terra::subset(drought_index, .) %>%
  terra::where.max(.) %>%
  {
    terra::xyFromCell(drought_index, .[2])
  }

middle_layer <- mean_di %>%
  filter(middle) %>%
  {
    drought_index[
      glue::glue("grids_germany_annual_drought_index_{.$year}17.asc")
    ]
  }

middle_xy <- middle_layer %>%
  as.data.frame() %>%
  unlist() %>%
  find_middle() %>%
  which() %>%
  terra::xyFromCell(middle_layer, .)

mid_year <- mean_di %>%
  filter(middle) %>%
  dplyr::pull(year)
min_year <- mean_di %>%
  filter(min) %>%
  dplyr::pull(year)
max_year <- mean_di %>%
  filter(max) %>%
  dplyr::pull(year)

# override location because station 15190 only has wind speed data
med_di_weather <- get_station_data(c(6.536500, 51.82930),
  min_date = as.Date(
    glue::glue("{year}-12-31",
      year = mid_year
    )
  )
) %>%
  clean_weather_data() %>%
  filter(year == mid_year)

min_di_weather <- get_station_data(min_xy,
  min_date = as.Date(
    glue::glue("{year}-12-31",
      year = min_year
    )
  )
) %>%
  clean_weather_data() %>%
  filter(year == min_year)

max_di_weather <- get_station_data(max_xy,
  min_date = as.Date(
    glue::glue("{year}-12-31",
      year = max_year
    )
  )
) %>%
  clean_weather_data() %>%
  filter(year == max_year)

usethis::use_data(min_di_weather, overwrite = TRUE)
usethis::use_data(med_di_weather, overwrite = TRUE)
usethis::use_data(max_di_weather, overwrite = TRUE)
