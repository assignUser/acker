#' sim_output UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sim_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("export_doy"), "Tag des Jahres für den Export", min = 1, max = 366, step = 1, value = 320),
    actionButton(ns("save_output"), "Daten Speichern"),
    textOutput(ns("saved"))
  )
}

#' sim_output Server Functions
#'
#' @noRd
sim_save_output <- function(id, results, base_sim, water_noise) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    get_result_selection <- function(result, export_doy) {
      result %>%
        filter(doy == export_doy) %>%
        select(NDS, LAI, WVEG, WTOP, WGRN, NDS, HI, starts_with("water_stress"))
    }
    create_layer <- function(values, name, base_raster, cells) {
      lyr <- terra::rast(base_raster)
      names(lyr) <- name
      lyr[cells] <- values
      lyr
    }

    out_rast <- reactiveVal()
    observe({
      akr <- water_noise$water
      cell_i <- terra::cells(akr)
      e_doy <- input$export_doy
      res <- results()
      tbl <- purrr::map_dfr(res, get_result_selection, e_doy) %>% dplyr::as_tibble()
      or <- tbl %>%
        as.list() %>%
        purrr::map2(., names(.), create_layer, akr, cell_i) %>%
        terra::rast()
      out_rast(or)
    }) %>% bindEvent(results())


    output$saved <- renderText({
      bs <- base_sim()
      or <- out_rast()

      file <- glue::glue("{bs$management$Fyear}_Mais_{input$export_doy}.tif")
      terra::writeRaster(or, file, overwrite = TRUE)

      glue::glue("Daten im aktuellen Verzeichnis gespeichert als: {file}")
    }) %>% bindEvent(input$save_output)
  })
}
