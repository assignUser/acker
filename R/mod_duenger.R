#' duenger UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_duenger_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' duenger Server Functions
#'
#' @noRd 
mod_duenger_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_duenger_ui("duenger_1")
    
## To be copied in the server
# mod_duenger_server("duenger_1")
