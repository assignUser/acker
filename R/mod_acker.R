#' acker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
acker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("osm_error")),
    textInput(ns("osm_id"), "Acker OSM ID", "395695875"),
    actionButton(ns("get_vec"), "Acker Daten abrufen"),
  )
}

#' acker Server Functions
#'
#' @noRd
acker_server <- function(id, water_noise) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    osm_err_txt <- reactiveVal("")
    acker <- reactive({
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
    water_noise <- acker_noise_server(id, acker, water_noise)
    return(list(
      acker = acker,
      water_noise  = water_noise,
      osm_id = reactive(input$osm_id)
    ))
  })
}


#' acker Server Functions
#'
#' @noRd
acker_noise_server <- function(id, acker, water_noise) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    water_noise$water_base <- reactive({
      # This resolution in degress for EPSG:4326 gives a cell area of about 1m^2 <- much too fine
      # 0.000012
      SQM_RES <- 0.0003
      ak <- acker() 
      akr <- terra::rast(ak, resolution = SQM_RES)
      terra::rast(akr, vals = acker_noise(akr, freq = 0.05)) %>% terra::mask(., ak)
    }) %>%
      bindEvent(acker(), ignoreNULL = TRUE)

    observe({
      water_noise$water <- water_noise$water_base()
    }) %>%
      bindEvent(water_noise$water_base())
    
    return(water_noise)
  })
}

