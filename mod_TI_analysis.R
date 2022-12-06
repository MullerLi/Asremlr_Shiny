#' SET_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TI_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">Single-Site Spatial Analysis</h1>'),
    # HTML('<h4 style="font-weight: bold; color: #00a65a;">Using ASReml</h4>'),
    fluidRow(
      column(width = 2,
             fluidRow(

               bs4Dash::box(width = 12,status = "success", solidHeader = FALSE,title = tagList(icon=icon("cogs"), "ASReml"),
                            selectInput(inputId=ns("variable"),
                                        label= tagList( "Phenotypic trait",
                                                        icon=tooltip(icon("question-circle"),
                                                                          title = "The column with the continous response variable.",
                                                                          placement = "top")),
                                        choices="", width = "100%"),
                            selectInput(inputId=ns("genotype"),
                                        label=tagList( "Genotype",
                                                       icon=tooltip(icon("question-circle"),
                                                                         title = "The column with genotypes.",
                                                                         placement = "top")),
                                        choices="", width = "100%"),
                            hr(),
                            selectInput(inputId=ns("DAP"),
                                        label=tagList( "DAP",
                                                       icon=tooltip(icon("question-circle"),
                                                                    title = "The column with genotypes.",
                                                                    placement = "top")),
                                        choices="", width = "100%"),

                            sliderInput(inputId=ns("timerange"),
                                        label=tagList( "timerange",
                                                       icon=tooltip(icon("question-circle"),
                                                                    title = "The column with genotypes.",
                                                                    placement = "top")),
                                        min=0, max=20, value=c(0,10),
                                        width = "100%")

               ),
               bs4Dash::box(width = 12,status = "success", solidHeader = FALSE,title = tagList(icon=icon("cogs"), "ASReml"),   # background = "light-blue"
                            selectInput(inputId=ns("smooth.method"),
                                        label= tagList( "smooth.method",
                                                        icon=tooltip(icon("question-circle"),
                                                                     title = "The column with the continous response variable.",
                                                                     placement = "top")),
                                        c("Direct" = "dir", "Log" = "log"),
                                        selected = 1, width = "100%"),

                            sliderInput(inputId = ns('smooth.df'),
                                        label = tagList( "smooth.df",
                                                         icon=tooltip(icon("question-circle"),
                                                                      title = "The column with genotypes.",
                                                                      placement = "top")),
                                        min = 1, max = 15, value = 8),

                            sliderInput(inputId = ns('envelope'),
                                        label = tagList( "envelope",
                                                         icon=tooltip(icon("question-circle"),
                                                                      title = "The column with genotypes.",
                                                                      placement = "top")),
                                        min = 0, max = 1, value = 0.2)
               ),
               bs4Dash::box(width = 12,status = "success", solidHeader = FALSE,title = tagList(icon=icon("cogs"), "ASReml"),   # background = "light-blue"
                            # fluidRow(
                            #   col_3(),
                            #   col_6(
                            #     actionBttn(inputId = ns("guide"),label = "Guide",style = "stretch",color = "warning",block = T, icon = icon("question-circle") )
                            #   ),
                            #   col_3()
                            # ),
                            selectInput(inputId=ns("variable_remain"),
                                        label= tagList( "variable_remain",
                                                        icon=tooltip(icon("question-circle"),
                                                                     title = "The column with the continous response variable.",
                                                                     placement = "top")),
                                        multiple = TRUE,
                                        choices="", width = "100%"),
                            fluidRow(
                              col_3(),
                              col_6(
                                downloadButton(ns("descargar"), "Download Plot", class="btn-success",
                                               style= " color: white ; background-color: #28a745"), br() ,
                                animate = shinyWidgets::animateOptions(
                                  enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                                  exit  = shinyWidgets::animations$fading_exits$fadeOutLeftBig
                                )
                              ),
                              col_3()
                            )
               ),
             )
      ),
      column(10,
             shinyjs::hidden(
               div(id=ns("only"),
                   fluidRow(
                     column(12,
                        fluidRow(
                          bs4TabCard(width = 12,id = "single_model_ti",maximizable = T,solidHeader = FALSE,closable = F,
                                     status ="success", side = "left", type = "tabs",

                                     tabPanel(title = "Original-Plot",active = T,
                                                shinycssloaders::withSpinner(plotlyOutput(ns("plot_original")),type = 5,color = "#28a745"),icon = icon("th")
                                     )
                          )
                        )
                      )
                   ),
                   fluidRow(
                     column(12,
                            fluidRow(
                              bs4TabCard(width = 12,id = "single_model_ti",maximizable = T,solidHeader = FALSE,closable = F,
                                         status ="success", side = "left", type = "tabs",

                                         tabPanel(title = "Original-Plot",active = T,
                                                  shinycssloaders::withSpinner(plotlyOutput(ns("plot_original")),type = 5,color = "#28a745"),icon = icon("th")
                                         )
                              )
                            )
                     )
                   )
               )
             )
      )
    )

  )
}

#' SET_analysis Server Function
#'
#' @noRd
mod_TI_analysis_server <- function(input, output, session, data){
  ns <- session$ns


  observeEvent(data$data(),{
    dt <- data$data()
    req(dt)
    updateSelectInput(session, "variable", choices=names(dt), selected = "Digital.biomass")
    updateSelectInput(session, "genotype", choices=names(dt),selected = "line_id")
    updateSelectInput(session, "DAP", choices=names(dt),selected = "DAP")
    updateSelectInput(session, "variable_remain", choices=names(dt))
  })

  observeEvent(input$DAP,{
    dt <- data$data()
    req(dt)
    ipDAP <- input$DAP
    req(ipDAP)
    DAP_for_timerange <-dt[, ipDAP]
    updateSliderInput(session, "timerange", value = c(min(DAP_for_timerange), max(DAP_for_timerange)),
                      min = min(DAP_for_timerange),
                      max = max(DAP_for_timerange))
  })


  download.data <- reactive({
    dt <- data$data()
    req(dt)
    variable <- input$variable
    variable_remain <- input$variable_remain
    genotype <- input$genotype
    DAP <- input$DAP
    timerange <- input$timerange

    dt_select <- dt %>% select(variable, genotype, DAP,input$variable_remain) %>% rename(genotype = genotype)
    left_join(newData.smooth(), dt_select, by = c("genotype", DAP)) %>% return()
  })


  output$descargar <- downloadHandler(
    filename = function() {
      paste("plotSpATS", input$variable,"csv", sep = ".")
    },
    content = function(file){
      write.csv(download.data(),file, row.names = F)
    }
  )



  data_filter <- function(data, variable, genotype, DAP, timerange){

    newdata <-data %>% dplyr::select(variable, genotype, DAP) %>%
      dplyr::filter(DAP >= (timerange[1]) & DAP <= (timerange[2]))
    colnames(newdata) <- c("variable", "genotype", "DAP")
    newdata <-newdata %>% dplyr::group_by(genotype, DAP) %>%
      summarise_at(vars(variable), list(variable = mean)) %>% ungroup
    return(newdata)
  }


  newData <- reactive({
    req(data$data())
    dt <- data$data()
    variable <- input$variable
    genotype <- input$genotype
    DAP <- input$DAP
    timerange <- input$timerange
    # req(variable)
    # req(genotype)
    # req(DAP)
    # req(timerange)
    data_return <- data_filter(dt, variable, genotype, DAP, timerange)
    return(data_return)

  })

  model.smoothed <- function(data, smooth.method, smooth.df){
    SET.dat <- data %>%
      splitContGRdiff(., responses = "variable",
                      INDICES="genotype", which.rates = c("AGR", "RGR"),
                      times.factor="DAP")
    SET.dat <- splitSplines(SET.dat, response = "variable", x ="DAP",
                            INDICES = "genotype",
                            smoothing.method = smooth.method, df = smooth.df)
    SET.dat <- splitSplines(SET.dat, response = paste0("variable",".AGR"), x ="DAP",
                            INDICES = "genotype",
                            smoothing.method = smooth.method, df = smooth.df)
    SET.dat <- splitSplines(SET.dat, response = paste0("variable",".RGR"), x ="DAP",
                            INDICES = "genotype",
                            smoothing.method = smooth.method, df = smooth.df)
    return(SET.dat)
  }

  newData.smooth <- reactive({
    newData <- newData()
    req(newData)
    smooth.df <- input$smooth.df %>% as.integer()
    req(newData)
    smooth.method <- input$smooth.method
    data_return <- model.smoothed(newData, smooth.method, smooth.df)
    return(data_return)

  })

  newData.median <- reactive({
    newData() %>% model.smoothed(., "direct", c(5,10))
  })

  ply_model <- function(data, type){
    plot_ly(
      data = data,
      x = ~ DAP,
      y = ~ paste0("variable",type) %>% get(),
      split = ~ genotype,
      type = 'scatter',
      mode = 'lines',
      connectgaps = TRUE)
    # )%>%
    #   layout(title = title,
    #          xaxis = list(title = "DAP"),
    #          yaxis = list(title = paste0(variable,type)))
  }


  # renderPlotly

  output$plot_original   <- renderPlotly({
     newData() %>%  ply_model(., "")
  })


}

## To be copied in the UI
# mod_SET_analysis_ui("SET_analysis_ui_1")

## To be copied in the server
# callModule(mod_SET_analysis_server, "SET_analysis_ui_1")

