#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  water_noise <- reactiveValues()
  acker_list <- acker_server("acker", water_noise)
  acker <- acker_list$acker
  water_noise <- acker_list$water_noise
  map_session <- map_server("map", acker, water_noise)
  hover_input <- map_hover_input("map")
  water_noise <- map_editor_server("editor", water_noise, hover_input)
  fertilizer_server("fertilizer", acker)
}
