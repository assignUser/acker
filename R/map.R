function() {
  ak <- get_acker_vect(729667622, 729394749,729394750)
  ak_sf <- get_acker_sf(729667622, 729394749,729394750)
  ctr <- get_acker_center(ak)
  bbox <- terra::ext(ak)[c(1, 3, 2, 4)]
  padding <- 0.00001
  vbox <- c(bbox[1:2] - padding, bbox[3:4] + padding)
    SQM_RES <- 0.000012
  akr <- terra::rast(ak, resolution = SQM_RES)
  akrn <- terra::rast(akr, vals = acker_noise(akr, freq = 0.003)) %>% terra::mask(., ak)
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::setView(ctr[1], ctr[2], 15) %>%
    leaflet::fitBounds(vbox[[1]], vbox[[2]], vbox[[3]], vbox[[4]]) %>%
    leaflet::addPolygons(data = ak_sf, fill = F) %>%
    leaflet::addRasterImage(akrn, colors = "viridis", opacity = 0.8)

}

# This resolution in degress for EPSG:4326 gives a cell area of about 1m^2
