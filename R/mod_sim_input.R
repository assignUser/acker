#' sim_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sim_input_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      actionButton(ns("start_sim"), "Simulation Starten") %>%
        prompter::add_prompt(
          position = "right",
          type = "warning",
          message = "Die Simulation kann mehrere Minuten dauern!"
        ),
      textOutput(ns("sim_text")) %>%
        shinycssloaders::withSpinner(color = "#0dc5c1", proxy.height = 20, id = "sim"),
      style = "margin-top: 15px;"
    ),
    mainPanel(
      col_6(
        h2("Wetter und Management"),
        uiOutput(ns("weather_ui")) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1", proxy.height = 20),
        selectInput(
          ns("select_find"),
          "Bestimmung Aussaattermin",
          list(
            "Fester Tag" = 0,
            "Fünfter regenfreier Tag" = 1
          )
        ),
        uiOutput(ns("search_win")),
        selectInput(
          ns("irrigation"),
          "Bewässerung",
          list(
            "Nur Regen" = 2,
            "Automatisch" = 1
          )
        ),
        numericInput(ns("start_doy"), "Erster möglicher Tag für die Aussaat", min = 1, step = 1, max = 366, value = 110),
        numericInput(ns("stop_doy"), "Spätester Tag des Jahres für Ernte", min = 1, step = 1, max = 366, value = 320)
      ), col_6(
        h2("Boden"),
        p(
          HTML(glue::glue(
            "Bodeninformationen können interaktive auf der ",
            "{enurl('https://www.lbeg.niedersachsen.de/karten_daten_publikationen/karten_daten/boden/bodenkarten/",
            "bodenkundliche_uebersichtskarte_150000/bodenuebersichtskarte-im-mastab-1--50-000-buek50-654.html', ' BÜK50')}",
            " gesucht und detailliert Boden Profile auf der Seite ",
            "{enurl('https://www.lbeg.de/extras/nlfbook/html/nds_main.htm', 'Böden in Niedersachsen')}",
            " nachgeschalgen werden."
          ))
        ),
        textInput(ns("soil_name"), "Name", value = "Braunerde"),
        textOutput(ns("soil_error")),
        numericInput(ns("GPV"), "Gesamt Poren Volumen (mm/dm)", min = 1, value = 41),
        numericInput(ns("LK"), "Luftkapazität (mm/dm)", min = 1, value = 10),
        numericInput(ns("nFK"), "Nutzbare Feldkapazität (mm/dm)", min = 1, value = 16),
        numericInput(ns("kf"), "Gesättigte Wasserleitfähigkeit (mm/d)", min = 1, value = 15),
        numericInput(ns("we"), "Effektive Durchwurzelungstiefe (cm)", min = 1, value = 100),
        numericInput(ns("soil_mod"), "Maximale Schwankung Bodenqualität (%)", min = 0, max = 99, value = 20)
      ),
      style = "padding-top: 15px;"
    )
  )
}

sim_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("result_out")) %>%
      shinycssloaders::withSpinner(color = "#0dc5c1", proxy.height = 20)
  )
}
#' sim_input Server Functions
#'
#' @noRd
sim_dynamic_inputs <- function(id, acker, env) {
  stopifnot(is.reactive(acker))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # as we can't lazy load we have to make sure the data is loaded
    minmax <- c("min", "med", "max")
    default_names <- glue::glue("{minmax}_di_weather")
    ensure_data(default_names, env)
    weather_ak <- reactive({
      ak <- acker()
      weather_ak <- get_acker_weather(ak)
    })

    output$search_win <- renderUI({
      if (input$select_find == 1) {
        return(tagList(numericInput(ns("search_dur"),
          "Länge des Suchfensters für den Aussaattermin.",
          min = 1,
          step = 1,
          value = 120
        )))
      }
      return(tagList())
    })

    output$weather_ui <- renderUI({
      default_choices <- list(
        "Trockenes Wetter (28)*" = default_names[[1]],
        "Normales Wetter (41)*" = default_names[[2]],
        "Feuchtes Wetter (51)*" = default_names[[3]]
      )
      ak <- acker()
      weather <- weather_ak()
      di <- get_acker_drought_index(ak)
      di_sel <- get_di_selection(weather, di)
      c_names <- with(di_sel, glue::glue("{year} ({round(di)})"))

      custom_choices <- do.call(list, as.list(as.numeric(di_sel$year)))
      names(custom_choices) <- c_names
      choices <- c(default_choices, custom_choices)

      return(tagList(
        selectInput(ns("weather"), "Wetter Auswahl", choices = choices)
        %>% prompter::add_prompt(
            position = "right", type = "info",
            message = glue::glue(
              "Die mit '*' markierten Einträge sind Standard Daten",
              " mit Extremwerten aus ganz Deutschland. \n Die Jahreszahlen sind individuell abgerufene Daten von ",
              "einer dem Acker möglichst nahen Wetterstation."
            )
          )
      ))
    }) %>% bindEvent(acker())

    return(weather_ak)
  })
}

sim_prepare_run <- function(id, acker, weather, water_noise) {
  stopifnot(is.reactive(acker))
  moduleServer(id, function(input, output, session) {
    sim_weather <- reactive({
      minmax <- c("min", "med", "max")
      default_names <- glue::glue("{minmax}_di_weather")
      ensure_data(default_names)
      all_w <- weather()
      w_choice <- input$weather
      validate(need(!is.null(input$weather), ""))

      if (is.na(as.numeric(w_choice))) {
        w_out <- get(w_choice)
      } else {
        w_out <- all_w %>% filter(year == as.numeric(w_choice))
      }
      w_out
    }) %>% bindEvent(input$weather, weather())

    soil <- reactive({
      validate(need(input$GPV >= input$nFK + input$LK, "GPV muss größer als nFK + LK sein!"))
      iCrop2R::german_soil(input$soil_name, input$GPV, input$LK, input$nFK, input$kf, input$we)
    })

    output$soil_error <- renderText({
      validate(need(input$GPV >= input$nFK + input$LK, "GPV muss größer als nFK + LK sein!"))
      ""
    })

    # Could be individualized for advanced users via config file in the future.
    loc <- list(tchng = 0, pchng = 1, CO2 = 385, VPDF = 0.75, lat = 38.5)
    corn <- iCrop2R::crops[22, ]
    management <- reactive({
      w <- sim_weather()
      list(
        Manag = "CornF",
        FixFind = as.numeric(input$select_find),
        Fyear = w$year[[1]],
        yrno = 1,
        SimDoy = input$start_doy,
        Fpdoy = input$start_doy,
        SearchDur = input$search_dur,
        RfreeP = 5,
        SowTmp = NULL,
        SowWat = NULL,
        water = as.numeric(input$irrigation),
        IRGLVL = 0.5,
        MAI1 = .9,
        MAI = .9,
        StopDoy = input$stop_doy,
        ClipNo = NULL,
        mnWH = NULL,
        mxWH = NULL
      )
    })

    base_sim <- reactiveVal()

    observe({
      a <- input$start_sim
      m <- management()
      s <- soil()
      w <- sim_weather()
      base_sim(iCrop2R::Simulation$new(corn, loc, m, s, w))
    }) %>% bindEvent(input$start_sim)

    sim_list <- reactiveVal()
    observe({
      soil_mod <- input$soil_mod / 100
      # Don't remove NAs so we can map values back to grid after sim
      noise_vals <- terra::values(water_noise$water, mat = FALSE)
      b_sim <- base_sim()
      sims <- noise_vals %>% purrr::map(function(x) {
        if (!is.na(x)) {
          sim_x <- b_sim$clone()
          fctr <- 1 + soil_mod * x

          sim_x$soil$saturation <- sim_x$soil$saturation * fctr
          sim_x$soil$drained_upper_limit <- sim_x$soil$drained_upper_limit * fctr
          sim_x$soil$extractable_water <- sim_x$soil$extractable_water * fctr
          sim_x$soil$lower_limit <- sim_x$soil$lower_limit * fctr
          sim_x$management$MAI <- sim_x$management$MAI * fctr
          sim_x$management$MAI <- sim_x$management$MAI1 * fctr
          x <- sim_x
        }

        x
      })
      sim_list(sims)
    }) %>% bindEvent(base_sim())

    results <- reactiveVal()
    observe({
      sl <- sim_list()
      res <- sl[7:9] %>% purrr::map( function(.x) {
        if (is.environment(.x)) {
          .x$run_simulation()
          return(.x$result)
        }
        NA
      })
      results(res)
    }) %>% bindEvent(sim_list())

    output$sim_text <- renderText({
      "Simulation beendet."
    }) %>% bindEvent(results(), input$start_sim, ignoreInit = TRUE)

    # TODO remove
    output$result_out <- DT::renderDT({
      r <- results()
      i <- which(!is.na(r)) %>% min()
      r[[i]]
    }) %>% bindEvent(results())

    list(
      results = results,
      base_sim = base_sim
    )
  })
}
