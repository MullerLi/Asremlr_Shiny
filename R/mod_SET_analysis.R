#' SET_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SET_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML('<h1 style="font-weight: bold; color: #00a65a;">Single-Site Spatial Analysis</h1>'),
    # HTML('<h4 style="font-weight: bold; color: #00a65a;">Using ASReml</h4>'),
    fluidRow(
      column(width = 2,
             fluidRow(
               bs4Dash::box(width = 12,status = "success", solidHeader = FALSE,title = tagList(icon=icon("cogs"), "ASReml"),   # background = "light-blue"
                            # fluidRow(
                            #   col_3(),
                            #   col_6(
                            #     actionBttn(inputId = ns("guide"),label = "Guide",style = "stretch",color = "warning",block = T, icon = icon("question-circle") )
                            #   ),
                            #   col_3()
                            # ),
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
                            # fluidRow(
                            #   col_3(),
                            #   col_6(
                            #     actionBttn(inputId = ns("ok"),label = "Fit Model",style = "jelly",color = "success",block = T, icon = icon("check") )
                            #   ),
                            #   col_3()
                            # )
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
                          bs4TabCard(width = 12,id = "single_model",maximizable = T,solidHeader = FALSE,closable = F,
                                     status ="success", side = "left", type = "tabs",

                                     tabPanel(title = "Original-Plot",active = T,
                                                shinycssloaders::withSpinner(plotlyOutput(ns("plot_original")),type = 5,color = "#28a745"),icon = icon("th")
                                     ),


                                     tabPanel(title = "Smoothed-Plot",active = T,
                                              shinycssloaders::withSpinner(plotlyOutput(ns("plot_smoothed")),type = 5,color = "#28a745"),icon = icon("th")
                                     ),

                                     tabPanel(title = "Median.dev-Plot",active = T,
                                              shinycssloaders::withSpinner(plotOutput(ns("plot_median")),type = 5,color = "#28a745"),icon = icon("th")
                                     )
                          )
                        )
                      )
                   ),
                   fluidRow(
                     column(12,
                            fluidRow(
                              bs4TabCard(width = 12,id = "single_model",maximizable = T,solidHeader = FALSE,closable = F,
                                         status ="success", side = "left", type = "tabs",

                                         tabPanel(title = "Original-Plot",active = T,
                                                  shinycssloaders::withSpinner(plotlyOutput(ns("plot_original_AGR")),type = 5,color = "#28a745"),icon = icon("th")
                                         ),


                                         tabPanel(title = "Smoothed-Plot",active = T,
                                                  shinycssloaders::withSpinner(plotlyOutput(ns("plot_smoothed_AGR")),type = 5,color = "#28a745"),icon = icon("th")
                                         ),

                                         tabPanel(title = "Median.dev-Plot",active = T,
                                                  shinycssloaders::withSpinner(plotOutput(ns("plot_median_AGR")),type = 5,color = "#28a745"),icon = icon("th")
                                         )
                              )
                            )
                     )
                   ),
                   fluidRow(
                     column(12,
                            fluidRow(
                              bs4TabCard(width = 12,id = "single_model",maximizable = T,solidHeader = FALSE,closable = F,
                                         status ="success", side = "left", type = "tabs",

                                         tabPanel(title = "Original-Plot",active = T,
                                                  shinycssloaders::withSpinner(plotlyOutput(ns("plot_original_RGR")),type = 5,color = "#28a745"),icon = icon("th")
                                         ),


                                         tabPanel(title = "Smoothed-Plot",active = T,
                                                  shinycssloaders::withSpinner(plotlyOutput(ns("plot_smoothed_RGR")),type = 5,color = "#28a745"),icon = icon("th")
                                         ),

                                         tabPanel(title = "Median.dev-Plot",active = T,
                                                  shinycssloaders::withSpinner(plotOutput(ns("plot_median_RGR")),type = 5,color = "#28a745"),icon = icon("th")
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
mod_SET_analysis_server <- function(input, output, session, data){
  # ns <- session$ns
  #
  # observeEvent(!input$able, toggle("first",anim = TRUE,time = 1,animType = "fade"))
  # observeEvent(!input$able2, toggle("second",anim = TRUE,time = 1,animType = "fade"))
  # observeEvent(!input$ar1ar1, toggle("inclu_ar1",anim = TRUE,time = 1,animType = "fade"))
  #
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
    download_01 <- left_join(newData.smooth(), dt_select, by = c("genotype", DAP))
    colnames(download_01)[c(3,5,6,7,8,9)] <- c(variable,
                                               paste0(variable,".AGR"),
                                               paste0(variable,".RGR"),
                                               paste0(variable,".smooth"),
                                               paste0(variable,".AGR.smooth"),
                                               paste0(variable,".RGR.smooth"))

    return(download_01)
  })


  output$descargar <- downloadHandler(
    filename = function() {
      paste("smoothed.parm", input$variable,"csv", sep = ".")
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

  median.dev.plot <- function(data, trait.types, smooth.df, envelope){

    #'## Probe the smoothing methods and DF
    smooth.dat <- probeSmoothing(data = data, response = "variable",
                                 xname =  "DAP" , times.factor= "DAP",
                                 individuals = "genotype",
                                 na.x.action = "exclude",
                                 na.y.action = "exclude",
                                 facet.x = ".", facet.y = ".",
                                 trait.types = c("response","AGR", "RGR"),
                                 smoothing.methods = c("dir","log"),
                                 df = smooth.df, x = "DAP", get.rates = TRUE,
                                 which.plots = "none", deviations.plots = "none")
    SET.dat <- merge(data, smooth.dat)
    # SET.dat <- merge(SET.dat, logist.sub, all.x = TRUE)
    SET.dat <- SET.dat %>% unique()

    #'## Plot the median deviations plots
    #+ "meddevn"
    plotMedianDeviations(data = SET.dat,
                         individuals = "genotype",
                         response = "variable",
                         response.smoothed = "variable.smooth",
                         x = "DAP",
                         xname = "DAP",
                         smoothing.methods = c("dir","log"),
                         df = smooth.df ,
                         facet.x = ".",
                         facet.y = ".",
                         trait.types = trait.types,
                         propn.types = envelope) %>% return()
  }


  # renderPlotly

  output$plot_original   <- renderPlotly({
     newData() %>%  ply_model(., "")
  })

  output$plot_smoothed  <- renderPlotly({
    newData.smooth() %>% ply_model(., ".smooth")

  })

  output$plot_median  <- renderPlot({
    newData.median() %>% median.dev.plot(., "response", c(5,10), input$envelope)
  })


  output$plot_original_AGR   <- renderPlotly({
    newData() %>% model.smoothed("direct", 5) %>% ply_model(., ".AGR")
  })

  output$plot_smoothed_AGR  <- renderPlotly({
    newData.smooth() %>% ply_model(., ".AGR.smooth")
  })

  output$plot_median_AGR  <- renderPlot({
    newData.median() %>% median.dev.plot(., "AGR", c(5,10), input$envelope)
  })

  output$plot_original_RGR   <- renderPlotly({
    newData() %>% model.smoothed("direct", 5) %>% ply_model(., ".RGR")
  })

  output$plot_smoothed_RGR  <- renderPlotly({
    newData.smooth() %>% ply_model(., ".RGR.smooth")
  })

  output$plot_median_RGR  <- renderPlot({
    newData.median() %>% median.dev.plot(., "RGR", c(5,10), input$envelope)
  })


  #
  # observeEvent(input$able,{
  #   dt <- data$data()
  #   req(dt)
  #   updateSelectInput(session, "block", choices=names(dt), selected = "")
  #   updateSelectInput(session, "incomplete", choices=names(dt), selected = "")
  # })
  #
  # observeEvent(input$able2,{
  #   dt <- data$data()
  #   req(dt)
  #   updateSelectInput(session, "cov1", choices=names(dt), selected = "")
  #   updateSelectInput(session, "cov2", choices=names(dt), selected = "")
  # })
  #
  # # preparing data for modelling
  # newData <- reactive({
  #   input$ok
  #   isolate({
  #     req(data$data())
  #     dt <- data$data()
  #     req(input$variable)
  #     req(input$genotype)
  #     req(input$column)
  #     req(input$row)
  #     variables <- list(response   = input$variable,
  #                       genotype   = input$genotype,
  #                       res_ran    = input$res_ran,
  #                       column     = input$column,
  #                       row        = input$row,
  #                       nugget     = input$nugget,
  #                       block      = input$block,
  #                       incomplete = input$incomplete,
  #                       block_ran  = input$block_ran,
  #                       cov1       = input$cov1,
  #                       cov2       = input$cov2,
  #                       add.block  = ifelse(input$block!="", T,F),
  #                       add.ibk    = ifelse(input$incomplete!="", T,F),
  #                       add.row    = ifelse("Row"%in%input$as_factors, T, F),
  #                       add.col    = ifelse("Column"%in%input$as_factors, T, F),
  #                       add.spl.row= ifelse("Row"%in%input$splines, T, F),
  #                       add.spl.col= ifelse("Column"%in%input$splines, T, F),
  #                       add.cov1   = ifelse(input$cov1!="", T,F),
  #                       add.cov2   = ifelse(input$cov2!="", T,F),
  #                       type.block = ifelse(input$block_ran, "random", 'fixed')
  #                       )
  #
  #     dt <- try(fill.asreml(dt, rows = conv_null(input$row), ranges = conv_null(input$column) ), silent = T)
  #     if(class(dt)=="try-error") dt <-  data.frame()
  #   })
  # return(list(components = variables, data = dt))
  # })
  #
  # # Modelo
  #
  # w <- Waiter$new(
  #   html = HTML("<center>",
  #               '<div class="dots-loader"></div>',
  #               "<br>","<br>","<br>",
  #               '<h5 style="font-weight: bold; color: grey;">Fitting ASreml Model...</h5>',
  #               "</center>"),
  #   color = transparent(0.3)
  # )
  #
  # # Single model
  # modelo <- reactive({
  #   req(newData())
  #   if(input$row==""|is.null(input$row)) return()
  #   if(input$column==""|is.null(input$column)) return()
  #   isolate({
  #     dt <- newData()$data
  #     comp <- newData()$components
  #     w$show()
  #     tryCatch(
  #       {
  #         model <- spatial.single(data=dt,
  #                                 gen      =  conv_null(comp$genotype),
  #                                 block    =  conv_null(comp$block),
  #                                 ibk      =  conv_null(comp$incomplete),
  #                                 row      =  conv_null(comp$row),
  #                                 col      =  conv_null(comp$column),
  #                                 cov1     =  conv_null(comp$cov1),
  #                                 cov2     =  conv_null(comp$cov2),
  #                                 resp     =  conv_null(comp$response),
  #                                 add.block=  comp$add.block,
  #                                 add.ibk  =  comp$add.ibk,
  #                                 add.row  =  comp$add.row,
  #                                 add.col  =  comp$add.col,
  #                                 add.spl.row =  comp$add.spl.row,
  #                                 add.spl.col =  comp$add.spl.col,
  #                                 add.cov1    =  comp$add.cov1,
  #                                 add.cov2    =  comp$add.cov2,
  #                                 add.nugget  =  comp$nugget,
  #                                 type.gen    =  ifelse(comp$res_ran, "random", 'fixed'),
  #                                 type.block  =  comp$type.block,
  #                                 type.residual= ifelse(input$ar1ar1, "ar1", "indep"))
  #       },
  #       error = function(e) {
  #         shinytoastr::toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-full-width",
  #                                   showMethod ="slideDown", hideMethod="hide", hideEasing = "linear")
  #       }
  #     )
  #     w$hide()
  #     if(!exists("model")) model <- NULL
  #
  #     return(model)
  #   })
  # })
  #
  #
  # observeEvent(input$ok,{
  #   if(!is.null(modelo())){
  #     show(id = "only", anim = TRUE, animType = "slide" )
  #   } else{
  #     hide(id = "only", anim = TRUE, animType = "slide" )
  #   }
  # }, ignoreInit = T, ignoreNULL = T)
  #
  # output$plot_spats <- renderPlot({
  #   input$ok
  #   isolate({
  #     req(modelo())
  #     model <- modelo()$mod
  #     spatial.ASReml(model, col = "col", row = "row", response = "resp", genotype = "gen" )
  #   })
  # })
  #
  # output$residuals_aug <- renderPlot({
  #   input$ok
  #   input$swicht1
  #   isolate({
  #     req(modelo())
  #     model <- modelo()$mod
  #     plot(model, spatial = ifelse(input$swicht1, "plot", "trend"))
  #   })
  # })
  #
  #
  # # Formula
  #
  # output$callModel <- renderPrint({
  #   input$ok
  #   isolate({
  #     req(modelo())
  #     model <- modelo()$mod
  #     model$call
  #   })
  # })
  #
  # output$summ <- renderPrint({
  #   input$ok
  #   isolate({
  #     req(modelo())
  #     model <- modelo()$mod
  #     summary(model)$varcomp %>%
  #       tibble::rownames_to_column("name") %>%
  #       dplyr::mutate_if(is.numeric, round, 3)
  #   })
  # })
  #
  # output$aov <- renderPrint({
  #   input$ok
  #   isolate({
  #     req(modelo())
  #     model <- modelo()$aov
  #     model %>%
  #       tibble::rownames_to_column("name") %>%
  #       dplyr::mutate_if(is.numeric, round, 3)
  #   })
  # })
  #
  #
  # output$INFO <-  function() {
  #   input$ok
  #   isolate({
  #      req(modelo())
  #      gt <- modelo()$gt
  #      gfit <- matrix(NA, ncol=9, nrow=1)
  #      gfit[1,1] <- c('Model')
  #      gfit[1,2] <- round(gt$n.vc, 3)
  #      gfit[1,3] <- round(gt$logL, 3)
  #      gfit[1,4] <- round(gt$aic, 3)
  #      gfit[1,5] <- round(gt$bic, 3)
  #      gfit[1,6] <- round(gt$herit.PEV, 3)
  #      gfit[1,7] <- round(gt$herit.VC, 3)
  #      gfit[1,8] <- round(gt$Aopt, 3)
  #      gfit[1,9] <- round(gt$Dopt , 3)
  #
  #      colnames(gfit) <- c('MODEL','n.VC','logL','AIC','BIC','herit-PEV','herit-VC','A-opt','D-opt')
  #      gfit <- data.frame(gfit)
  #      gfit  %>%
  #        dplyr::select(MODEL, everything()) %>%
  #        kableExtra::kable(escape = F, align = "c") %>%
  #        kableExtra::kable_styling(c("hover","responsive","condensed"), full_width = T, position = "center")
  #   })
  # }
  #
  # # observeEvent(newData()$components$nugget, toggle("swicht1",anim = TRUE,time = 1,animType = "fade"))
  # # observeEvent(newData()$components$nugget, toggle("swicht2",anim = TRUE,time = 1,animType = "fade"))
  #
  # observe({
  #   toggle(id = "swicht1", condition = length(grep("units", modelo()$mod$call))!=0 )
  #   toggle(id = "swicht2", condition = length(grep("units", modelo()$mod$call))!=0 )
  # })
  #
  # output$semivariogram <- renderPlot({
  #   input$ok
  #   input$swicht2
  #   isolate({
  #     req(modelo())
  #     model <- modelo()$mod
  #     # DATA <- data.frame(model$mf)
  #     # DATA$residuals <- residuals(model, spatial =  ifelse(input$swicht2, "plot", "trend"))
  #     # DATA$col <- as.numeric(DATA$col)
  #     # DATA$row <- as.numeric(DATA$row)
  #     # ic <- which(names(DATA)=="col")
  #     # ir <- which(names(DATA)=="row")
  #     # ix <- which(names(DATA)=="residuals")
  #     # DATA <- geoR::as.geodata(DATA, coords.col = c(ic,ir),  data.col = ix)
  #     # par(mfrow=c(1,2))
  #     # var1 = geoR::variog(DATA, max.dist=1000)
  #     # plot(var1)
  #     # env.var = geoR::variog.mc.env(DATA, obj.v=var1, nsim=100)
  #     # plot(var1, env=env.var)
  #     plot(asreml::varioGram.asreml(model,
  #                                   spatial =  ifelse(input$swicht2, "plot", "trend")),
  #          main = paste("Empirical Variogram ( Residuals:", ifelse(input$swicht2, "plot", "trend"), ")" ))
  #   })
  # })
  #
  # # GUIA
  # # observeEvent(input$guide,
  # #              rintrojs::introjs(session,options = list("nextLabel"="Next",
  # #                                                       "prevLabel"="Back",
  # #                                                       "skipLabel"="Skip")))
  #
  # # observeEvent(input$ok,{
  # #   print(newData()$components)
  # #   # print(modelo()$string)
  # #   # print(modelo()$gt)
  # # })
  #
  #
  # # Download PLOT SPATIAL
  # output$descargar <- downloadHandler(
  #   filename = function() {
  #     paste("plotSpATS", input$typefile, sep = ".")
  #   },
  #   content = function(file){
  #     if(input$typefile=="png") {
  #       png(file,width = input$png.wid ,height = input$png.hei)
  #       spatial.ASReml(modelo()$mod, col = "col", row = "row", response = "resp", genotype = "gen" )
  #       dev.off()
  #     } else {
  #       pdf(file,width = input$pdf.wid , height = input$pdf.hei )
  #       spatial.ASReml(modelo()$mod, col = "col", row = "row", response = "resp", genotype = "gen" )
  #       dev.off()
  #     }
  #   }
  # )
  #
  #
  # return(list(
  #   model = modelo,
  #   run = reactive(input$ok)
  # ))


}

## To be copied in the UI
# mod_SET_analysis_ui("SET_analysis_ui_1")

## To be copied in the server
# callModule(mod_SET_analysis_server, "SET_analysis_ui_1")

