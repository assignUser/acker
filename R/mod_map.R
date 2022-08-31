#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
map_reset_button <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("reset_view"), "Ansicht zurücksetzten")
  )
}

#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"), height = 800)
  )
}

#' map Server Functions
#'
#' @noRd
map_server <- function(id, acker, water_noise) {
  moduleServer(id, function(input, output, session) {
    blues <- leaflet::colorNumeric("Blues", c(-1, 1), na.color = NA)
    
    output$map <- leaflet::renderLeaflet(
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addLegend(
          pal = blues, values = c(seq(-1, 1, .1)),
          title = "Bodenqualität"
        )
    )

    map_clear_all(id, acker, water_noise, session)
    map_reset_view(id, acker, session)

    observe({
      ak <- acker()
      noise <- water_noise$water %>%
        terra::disagg(15, method = "bilinear") %>%
        terra::mask(.,ak)

      leaflet::leafletProxy("map", session = session) %>%
        leaflet::addRasterImage(
          noise,
          colors = blues,
          opacity = 0.8
        )
    }) %>%
      bindEvent(water_noise$water, acker())

    return(session)
  })
}

#' Reset Map View on Acker
#'
#' @noRd
map_reset_view <- function(id, acker, ses) {
  stopifnot(is.reactive((acker)))
  moduleServer(id, function(input, output, session) {
    observe({
      ak <- acker()
      bbox <- terra::ext(ak)[c(1, 3, 2, 4)]
      ctr <- get_acker_center(ak)
      padding <- 0.00001
      vbox <- c(bbox[1:2] - padding, bbox[3:4] + padding)
      leaflet::leafletProxy("map", session = ses) %>%
        leaflet::setView(ctr[1], ctr[2], 15) %>%
        leaflet::fitBounds(vbox[[1]], vbox[[2]], vbox[[3]], vbox[[4]])
    }) %>%
      bindEvent(acker(), input$reset_view)
  })
}

#' Clear map
#'
#' @noRd
map_clear_all <- function(id, acker, water_noise, ses) {
  moduleServer(id, function(input, output, session) {
    observe({
      ak <- acker()
      noise <- water_noise$water
      edit_raster <- terra::as.polygons(noise, trunc = FALSE, dissolve = FALSE)
      leaflet::leafletProxy("map", session = ses) %>%
        leaflet::clearImages() %>%
        leaflet::clearShapes() %>%
        leaflet::addPolygons(data = ak, fill = FALSE, color = "#000") %>%
        leaflet::addPolygons(data = edit_raster, fill = FALSE, opacity = 0)
    }) %>%
      bindEvent(acker())
  })
}

#' Return hover inputs
#'
#' @noRd
map_hover_input <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$map_shape_mouseover)
  })
}
