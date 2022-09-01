#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      prompter::use_prompt(),
      titlePanel("ackeR"),
      tabsetPanel(id = "all_tabs",
        tabPanel(
          "Map",
          sidebarLayout(
            sidebarPanel(
              acker_ui("acker"),
              map_reset_button("map-map"),
              map_editor_ui("editor"),
              style = "margin-top: 15px;"
            ),
            mainPanel(
              map_ui("map"),
              style = "margin-top: 15px;"
            )
          )
        ),
        tabPanel(
          "Düngung",
          sidebarLayout(
            sidebarPanel(fertilizer_settings("fertilizer"), 
              style = "margin-top: 15px;"
            ),
            mainPanel(fertilizer_ui("fertilizer"), style = "padding-top: 15px;")
          )
        ),
        tabPanel(
          "Simulation Inputs",
          sim_input_ui("simput")
        ),
        tabPanel("Results",
        sim_output_ui2("sim_out"),
      sim_output_ui("simput"))
      ),
      style = "max-width: 2000px;"
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ackeR"
    ),
    tags$style(HTML(
      "[class*=hint--][aria-label]:after {
        white-space: pre;
       }"
    )),
    # Add keypress hooks with hardcoded namespace
    tags$script('
      $(document).keydown( function (e) {
    Shiny.onInputChange("editor-key_press", {
      ctrl: e.ctrlKey,
      shift: e.shiftKey
      });
    });
  '),
    tags$script('
    $(document).keyup( function (e) {
       Shiny.onInputChange("editor-key_press",  {
        ctrl: false,
        shift: false
       });
    });
  ')
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
