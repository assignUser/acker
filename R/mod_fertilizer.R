#' fertilizer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
fertilizer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("fert_text"), style = "max-width: 500px;")
  )
}
#' fertilizer settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
fertilizer_settings <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("n_min"), "Nmin", value = 60, min = 0),
    numericInput(ns("n_soll"), "Nsoll", value = 220, min = 1),
    numericInput(ns("n_range"), "Maximaler Teilflächen Nmin überschuss (%) ", value = 40, min = 0, max = 100),
    numericInput(ns("bgr_m3"), "Biogasgärreste (m³/ha)", value = 40, min = 1),
    numericInput(ns("bgr_nh4"), "NH₄-N (kg/m³)", value = 3.2, min = .1),
    selectInput(ns("bgr_eff"), "BGR N-Pflanzenverfügbarkeit",
      choices = list("Konventionelle Ausbringung - 75%" = .75, "Gülledrill - 100%" = 1)
    ),
    numericInput(ns("kas_price"), "KAS27 Preis €/t", value = 700, min = 1),
    actionButton(ns("calc_n"), "Neuberechnen")
  )
}
#' fertilizer Server Functions
#'
#' @noRd
fertilizer_server <- function(id, acker) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(acker))
    ns <- session$ns

    n_noise <- reactive({
      ak <- acker()
      # ~40m² cell size
      akr <- terra::rast(ak, resolution = 0.00007)
      terra::rast(akr, vals = acker_noise(akr, freq = 0.05, r = c(0, input$n_range / 100))) %>% terra::mask(ak)
    })

    output$fert_text <- renderUI({
      validate(
        need(input$n_min <= input$n_soll, "Bitte sinnvolle werte für Nmin und Nsoll eingeben!")
      )
      ak <- acker()
      n_noise <- n_noise()
      kas_n <- .27
      ha <- terra::expanse(n_noise, unit = "ha") %>% round(2)
      cells_ha <- terra::cellSize(n_noise, unit = "ha") %>% terra::values(na.rm = TRUE)

      bgr_n <- input$bgr_nh4 * as.numeric(input$bgr_eff) * bgr_m3
      normal_fert <- input$n_soll - input$n_min - bgr_n
      n_mod <- 1 + terra::values(n_noise, na.rm = TRUE)
      precise_nmin <- input$n_min * n_mod
      precise_fert <- input$n_soll - precise_nmin - bgr_n

      kg_kas <- round((normal_fert / kas_n) * ha, 2) %>% round()
      kg_kas_precise <- sum((precise_fert / kas_n) * cells_ha) %>% round()
      kg_diff <- round(kg_kas - kg_kas_precise)
      kg_diff_price <- round(kg_diff / 1000 * input$kas_price)
      text1 <- glue::glue(
        "Die ausgewählte Anbaufläche umfasst {ha} ha. ",
        "Bei einem Düngeplan der den Einsatz von Biogasgärresten und eine ",
        "mineralische Unterfußdüngung vorsieht würden auf konventionelle Weise ",
        "{kg_kas} kg KAS27 mit einem Preis von {round(kg_kas/1000 * input$kas_price)}€ benötigt."
      )

      text2 <- glue::glue(
        "Bei einer teilflächenspezifischen Düngung würden hingegen ",
        "nur {kg_kas_precise} kg KAS27 benötigt.",
        "Das ist eine Einsparung von {kg_diff_price}€ für diese Anbaufläche oder {round(kg_diff/ha, 2)} kg/ha bzw. {round(kg_diff_price/ha, 2)}€/ha."
      )
      tagList(
        p(text1),
        br(),
        p(text2)
      )
    }) %>% bindEvent(input$calc_n, acker())
  })
}

## To be copied in the UI
# mod_fertilizer_ui("fertilizer_1")

## To be copied in the server
# mod_fertilizer_server("fertilizer_1")
