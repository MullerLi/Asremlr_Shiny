#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinyjs waiter ggplot2 dplyr summarytools plotly growthPheno reshape2
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # sever::sever()


  # observeEvent(input$toAwesome00, {updatebs4TabItems(session = session, inputId = "tabs", selected = "home")})
  # observeEvent(input$toAwesome11, {updatebs4TabItems(session = session, inputId = "tabs", selected = "Data")})
  observeEvent(input$toAwesome22, {updatebs4TabItems(session = session, inputId = "tabs", selected = "modelo")})
  # observeEvent(input$toAwesome33, {updatebs4TabItems(session = session, inputId = "tabs", selected = "valueboxes")})

  # Import Data
  data <- callModule(mod_import_dt_server, "import_dt_ui_1")
  data

  data1 <- callModule(mod_import_dt_ti_server, "import_dt_ti_ui_1")
  data1

  # Descriptives
  callModule(mod_descrip_raw_server, "descrip_raw_ui_1", data = data, plot = 1)
  callModule(mod_descrip_raw_server, "descrip_raw_ui_2", data = data, plot = 2)

  # Distribution
  callModule(mod_distribution_server, "distribution_ui_1", data = data)



  # Augmented
  aug <- callModule(mod_aug_model_server, "aug_model_ui_1",  data = data)
  aug
  callModule(mod_aug_result_server, "aug_result_ui_1", model = aug)


  # ASReml
  ASRml <- callModule(mod_spats_asreml_server, "spats_asreml_ui_1", data = data)
  ASRml
  callModule(mod_spats_asreml_effects_server, "spats_asreml_effects_ui_1", model = ASRml)

  # # ASReml Selector
  # selector <- callModule(mod_asreml_selector_server, "asreml_selector_ui_1", data = data)
  # selector
  # callModule(mod_asreml_selector_effects_server, "asreml_selector_effects_ui_1", model = selector)

  # SET_analysis
  SET <- callModule(mod_SET_analysis_server, "SET_analysis_ui_1", data = data)
  SET

  # TI_analysis
  TI <- callModule(mod_TI_analysis_server, "TI_analysis_ui_1", data = data1)
  TI
  # callModule(mod_spats_asreml_effects_server, "SET_analysis_effects_ui_1", model = SET)


}
