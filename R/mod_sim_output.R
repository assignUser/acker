#' sim_output UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sim_output_ui2 <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("sim_summary"))
  )
}

#' sim_output Server Functions
#'
#' @noRd
sim_save_output <- function(id, results, base_sim, water_noise) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$sim_summary <- renderTable({
      bs <- base_sim()
      bs$run_simulation()
      base_result <- bs$result
      days <- nrow(base_result)
      summary <- base_result %>% dplyr::summarise(
        mature = any(MAT == 1),
        mature_doy = ifelse(mature, doy[[which(MAT == 1) %>% min()]], NA),
        mature = mature && mature_doy != bs$management$StopDoy,
        mature_doy = ifelse(mature, mature_doy, NA),
        max_LAI = max(LAI),
        max_NDS = min(max(NDS), 1),
        min_doy = min(doy),
        max_doy = max(doy),
        year = unique(year),
        max_WVEG = max(WVEG),
        max_WTOP = max(WTOP),
        max_WGRN = max(WGRN),
        max_HI = max(HI),
        sum_irrigation = sum(irrigation_mm)
      )
       r <- results()
      # i <- which(!is.na(r)) %>% min()
      # r[[i]]
      summary
    }) 
  })
}
