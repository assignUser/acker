#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic
    fluidPage(
      prompter::use_prompt(),
      titlePanel("ackeR"),
      tabsetPanel(
        tabPanel(
          "Map",
          sidebarLayout(
            sidebarPanel(
              acker_ui("acker"),
              map_reset_button("map-map"),
              map_editor_ui("editor")
            ),
            mainPanel(
              map_ui("map")
            )
          )
        ),
        tabPanel("Düngung"),
        tabPanel("Simulation Inputs"),
        tabPanel("Settings")
      ),
     style='max-width: 2000px;')
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
      app_title = "acker"
    ),
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