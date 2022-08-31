function() {
  ak <- get_acker_vect(c(729667622, 729394749, 729394750))
  # ak_sf <- get_acker_sf(729667622, 729394749, 729394750)
  ctr <- get_acker_center(ak)
  bbox <- terra::ext(ak)[c(1, 3, 2, 4)]
  padding <- 0.00001
  vbox <- c(bbox[1:2] - padding, bbox[3:4] + padding)
  # This resolution in degress for EPSG:4326 gives a cell area of about 1m^2
  SQM_RES <- 0.0003#0.000052
  akr <- terra::rast(ak, resolution = SQM_RES)
  akrn2 <- terra::rast(akr, vals = acker_noise(akr, freq = 0.05))  %>% terra::mask(ak)
  akrv <- terra::as.polygons(akrn2, trunc = F, dissolve = F)
   akrn <- akrn2 %>% terra::disagg(15, method = "bilinear")%>%
    terra::mask(., ak)
   file <- tempfile(fileext = ".tif")
   terra::writeRaster(akrn, file)
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::setView(ctr[1], ctr[2], 15) %>%
    leaflet::fitBounds(vbox[[1]], vbox[[2]], vbox[[3]], vbox[[4]]) %>%
    leaflet::addPolygons(data = ak, fill = F) %>%
    leaflet::addPolygons(data = akrv, fill = F, stroke = F) %>%
    leaflet::addRasterImage(akrn, colors = "Blues", opacity = 0.8)


    soil_mod <- .2
    sim <- iCrop2R::Simulation$new(crop, loc, manag, default_soil, max_di_weather)
    vals <- terra::values(akrn2, mat = FALSE)
    simulations <- vals %>% purrr::map(function(x) {
      if(!is.na(x)) {
        sim_x <- sim$clone()
        fctr <- 1 + soil_mod * x

        sim_x$soil$saturation <- sim_x$soil$saturation * fctr
        sim_x$soil$drained_upper_limit <- sim_x$soil$drained_upper_limit * fctr
        sim_x$soil$extractable_water <- sim_x$soil$extractable_water * fctr
        sim_x$soil$lower_limit <- sim_x$soil$lower_limit * fctr
        sim_x$management$MAI <- sim_x$management$MAI * fctr
        sim_x$management$MAI <- sim_x$management$MAI1 * fctr
        x <- sim_x
      }
      x
    })
    res <- purrr::map(simulations, function(.x) {
      if(!any(is.na(.x))) {
        .x$run_simulation()
        return(.x$result[1,])
      }
      NA
    })
n_min <- 80
bgr_m3 <- 40
bgr_nh4 <- 3.2 * .75 * bgr_m3
n_soll <- 220 
mineral_fert <- n_soll - n_min - bgr_nh4
min_fert <- n_soll - n_min * (1 + terra::values(akrn2, na.rm = TRUE)) - bgr_nh4
kas_n <- .27
ha <- terra::expanse(akrn2, "ha")
kg_kas <- (mineral_fert / kas_n) * ha
kg_kas_actual <-{ (min_fert / kas_n) *  (terra::cellSize(akrn2, unit =  "ha") %>% terra::values(na.rm = TRUE))} %>% sum()

}

