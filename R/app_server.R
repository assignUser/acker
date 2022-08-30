#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  output$map <- leaflet::renderLeaflet(
    leaflet::leaflet() %>%
      leaflet::addTiles()
  )

  noise_layers <- reactiveValues()
  # Cache buster
  osm_err_txt <- reactiveVal("")
  acker <- reactive({
    # noise_layers$water_id <- 0
    if (input$osm_id == "395695875") {
      osm_err_txt("")
      return(default_acker)
    }

    osm_ids <- input$osm_id %>%
      strsplit(",") %>%
      unlist() %>%
      as.numeric()

    valid_input <- !is.null(osm_ids) && !any(is.na(osm_ids))
    txt <- "Bitte ein oder mehrere numerische, kommaseparierte OSM IDs eingeben!"
    # re-route error msg
    if (!valid_input) {
      osm_err_txt(txt)
    } else {
      osm_err_txt("")
    }

    validate(
      need(valid_input, txt)
    )

    ak <- try(get_acker_vect(osm_ids), silent = TRUE)
    if (inherits(ak, "try-error")) {
      # TODO show message
      print(ak)
      ak <- default_acker
      osm_err_txt(glue::glue(
        "Es gab ein Problem beim abrufen der Ackerdaten von Overpass.",
        "\\nStandard Acker wurde geladen. Bitte später nochmal versuchen."
      ))
    } else {
      osm_err_txt("")
    }

    ak
  }) %>%
    bindEvent(input$get_vec, ignoreNULL = FALSE)

  output$osm_error <- renderText(osm_err_txt())

  noise_layers$water_base <- reactive({
    # This resolution in degress for EPSG:4326 gives a cell area of about 1m^2 <- much too fine
    # 0.000012
    SQM_RES <- 0.0003
    ak <- acker()
    akr <- terra::rast(ak, resolution = SQM_RES)
    terra::rast(akr, vals = acker_noise(akr, freq = 0.05)) %>% terra::mask(., ak)
  }) %>%
    bindCache(input$osm_id) %>%
    bindEvent(acker())

  observe({
    noise_layers$water <- noise_layers$water_base()
  }) %>%
    bindEvent(noise_layers$water_base())

  # Noise map editor
  observe({
    if (edit_add() || edit_remove()) {
      current <- noise_layers$water
      pos <- cbind(
        input$map_shape_mouseover$lng,
        input$map_shape_mouseover$lat
      )

      cell <- terra::cellFromXY(current, pos)
      cells_adj <- terra::adjacent(current, cell, directions = "queen") %>% unlist()

      value_ctr <- terra::extract(current, cell) %>% unlist()
      values_adj <- terra::extract(current, cells_adj) %>% unlist()
      values_old <- c(value_ctr, values_adj)

      mod <- input$edit_strength %>% as.numeric()
      if (edit_add()) {
        value_ctr <- min(value_ctr + mod, 1)
        values_adj <- pmin(values_adj + mod / 2, 1)
      } else if (edit_remove()) {
        value_ctr <- max(value_ctr - mod, -1)
        values_adj <- pmax(values_adj - (mod / 2), -1)
      }

      cells <- c(cell, cells_adj)
      values <- c(value_ctr, values_adj)

      noise_layers$water[cells] <- values
    }
  }) %>%
    bindEvent(input$map_shape_mouseover)

  bounding_box <- reactive({
    ak <- acker()
    terra::ext(ak)[c(1, 3, 2, 4)]
  }) %>% bindEvent(acker())

  # focus on acker
  observe({
    ak <- acker()
    bbox <- bounding_box()
    ctr <- get_acker_center(ak)
    padding <- 0.00001
    vbox <- c(bbox[1:2] - padding, bbox[3:4] + padding)
    leaflet::leafletProxy("map") %>%
      leaflet::setView(ctr[1], ctr[2], 15) %>%
      leaflet::fitBounds(vbox[[1]], vbox[[2]], vbox[[3]], vbox[[4]])
  }) %>%
    bindEvent(acker(), input$reset_view)

  # add outline
  observe({
    ak <- acker()
    noise <- noise_layers$water
    edit_raster <- terra::as.polygons(noise, trunc = FALSE, dissolve = FALSE)
    leaflet::leafletProxy("map") %>%
      leaflet::clearImages() %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
      leaflet::addPolygons(data = ak, fill = FALSE, color = "#000") %>%
      leaflet::addPolygons(data = edit_raster, fill = FALSE, opacity = 0)
  }) %>%
    bindEvent(acker())


  # add noise layer changes
  blues <- leaflet::colorNumeric("Blues", c(-1, 1), na.color = NA)
  # add Legend only once
  observe({
    leaflet::leafletProxy("map") %>%
      leaflet::addLegend(
        pal = blues, values = c(seq(-1, 1, .1)),
        title = "Bodenqualität"
      )
  }) %>% bindEvent(acker())


  observe({
    ak <- acker()
    noise <- noise_layers$water %>%
      terra::disagg(15, method = "bilinear") %>%
      terra::mask(., ak)

    leaflet::leafletProxy("map") %>%
      leaflet::addRasterImage(
        noise,
        colors = blues,
        opacity = 0.8
      )
  }) %>%
    bindEvent(noise_layers$water, acker())

  # Add/remove from noise layer
  edit_add <- reactive({
    !is.null(input$key_press) && input$key_press$ctrl
  })
  edit_remove <- reactive({
    !is.null(input$key_press) && input$key_press$shift
  })
}
