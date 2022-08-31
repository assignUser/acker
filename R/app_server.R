#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  acker_list <- acker_server("acker")
  acker <- acker_list$acker
  noise_layers <- acker_list$noise_layers
  map_session <- map_server("map", acker, noise_layers)
  hover_input <- map_hover_input("map")
  noise_layers <- map_editor_server("editor", noise_layers, hover_input)
}
