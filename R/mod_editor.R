#' editor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
map_editor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("edit_strength"), "Pinsel Stärke",
      choices = list(schwach = 0.1, mittel = 0.2, stark = 0.3), selected = .2,
      inline = TRUE
    ) %>%
      prompter::add_prompt(
        position = "bottom/right", type = "info",
        message = glue::glue(
          "Zum modifizieren der Bodenqualität 'Shift' (-) oder 'Strg' (+) ",
          "gedrückt halten und durch die zu verändernden Bereiche fahren."
        )
      )
  )
}

#' editor Server Functions
#'
#' @noRd
map_editor_server <- function(id, noise_layers, hover_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Noise map editor

    # The name spacing for key_press is hardcoded in app_ui.R
    edit_add <- reactive({
      !is.null(input$key_press) && input$key_press$ctrl
    })

    edit_remove <- reactive({
      !is.null(input$key_press) && input$key_press$shift
    })

    observe({
      # print(print(edit_add()))
      if (edit_add() || edit_remove()) {
        current <- noise_layers$water
        pos <- cbind(
          hover_input()$lng,
          hover_input()$lat
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
      bindEvent(hover_input())

    noise_layers
  })
}
