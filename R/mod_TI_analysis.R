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
               bs4Dash::box(width = 12,status = "success", solidHeader = FALSE,title = tagList(icon=icon("cogs"), "ASReml"),   # background = "light-blue"

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
               bs4Dash::box(width = 12,status = "success", solidHeader = FALSE,title = tagList(icon=icon("cogs"), "Smoothing by time interval"),   # background = "light-blue"

                            radioButtons(inputId=ns("radiobutton"),
                                         "Choose one:",
                                         choiceNames = list(
                                           "equal","uequal"
                                         ),
                                         choiceValues = list(
                                           "equal", "uequal"
                                         )),

                              sliderInput(inputId=ns("time.interval"),
                                          label= tagList( "time interval",
                                                          icon=tooltip(icon("question-circle"),
                                                                       title = "The column with the continous response variable.",
                                                                       placement = "top")),
                                          min=0, max=20, value=c(0),width = "100%"),

                              textInput(inputId=ns("bin"),
                                          label= tagList( "time bin",
                                                          icon=tooltip(icon("question-circle"),
                                                                       title = "The column with the continous response variable.",
                                                                       placement = "top")),
                                          width = "100%")

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
                                                  shinycssloaders::withSpinner(plotlyOutput(ns("plot_interval")),type = 5,color = "#28a745"),icon = icon("th")
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

    dt_select <- dt %>% select(genotype,input$variable_remain) %>% rename(genotype = genotype) %>% unique

    dcast_DAP <- intervalData() %>% dplyr::select(genotype, DAP_interval, variable) %>% unique  %>% dcast(genotype   ~ DAP_interval, value.var = "variable")

    left_join(dcast_DAP, dt_select, by = c("genotype")) %>% return()
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

#####################################
interval.data <- function(data, smooth.parm, bin, radiobutton){


  intervalmean_bin <- function(DAP_series, bin){
    require(dplyr)
    bin_new <- strsplit(bin, split = ",") %>% unlist %>% as.integer()
    head_tail <- data.frame(head = bin_new[-(length(bin_new))], tail = bin_new[-1]-1)
    head_tail$tail[length(bin_new)-1] <- head_tail$tail[length(bin_new)-1]+1
    a <- data$DAP %>% unique %>% sort()
    group_a <- data.frame(a, group = NA, DAP_interval = NA)
    for(i in 1: nrow(head_tail)){
      group_a$group [which(group_a$a>=head_tail[i,1]  &  group_a$a<=head_tail[i,2])] <- i
      group_a$DAP_interval [which(group_a$a>=head_tail[i,1]  &  group_a$a<=head_tail[i,2])] <- paste0(head_tail[i,1], "-", head_tail[i,2])
    }
    group_summarise <- group_a %>% group_by(group)%>% summarise(groupmean = mean(a))
    group_join <- left_join(group_a, group_summarise, by = "group")
    group_join2 <- left_join(data.frame(a= data$DAP), group_join, by = "a") %>% dplyr::select(-group) %>% rename(DAP=a, DAP_group=groupmean)
    return(group_join2)
  }



    intervalmean <- function(DAP_series, smooth.parm){
        require(dplyr)
        a <- data$DAP %>% unique %>% sort()
        b <- smooth.parm
        mod <- length(a) %/% b
        remain <- length(a) %% b
        group <- c(rep(1:mod, each=b),rep(mod+1, remain))

        group_a <- data.frame(a, group, DAP_interval = NA)
        for(i in 1: mod){
          select_a <- group_a$a[ which(group_a$group == i)]
          group_a$DAP_interval[ which(group_a$group == i)] <- paste0(min(select_a),"-", max(select_a))
        }
        group_summarise <- group_a %>% group_by(group)%>% summarise(groupmean = mean(a))
        group_join <- left_join(group_a, group_summarise, by = "group")
        group_join2 <- left_join(data.frame(a= data$DAP), group_join, by = "a") %>% dplyr::select(-group) %>% rename(DAP=a, DAP_group=groupmean)
        return(group_join2)
    }

    date.smooth <- function(data, smooth.parm, bin, radiobutton){
        data_filted <- data

        if(radiobutton == "equal"){
          data_intervalmean <- intervalmean(data_filted$DAP, smooth.parm)
        }else{
          data_intervalmean <- intervalmean_bin(data_filted$DAP, bin)
        }

        data_filted <- left_join(data_filted, data_intervalmean, by = "DAP")
        data_filted <- data_filted %>% group_by(genotype, DAP_group, DAP_interval) %>% mutate(variable= mean(variable)) %>% unique
        data_filted <- data_filted %>% ungroup()
        data_filted <- data_filted %>% filter(!is.na(DAP_interval))
        return(data_filted)
    }
  return(date.smooth(data,smooth.parm %>% as.integer(),  bin, radiobutton))
}

intervalData <- reactive({
    smooth.parm <- input$time.interval
    bin <- input$bin
    radiobutton <- input$radiobutton
    interval.data <- interval.data(newData(), smooth.parm, bin, radiobutton)
    return(interval.data)
  })


#####################################
  ply_model <- function(data, type){
    plot_ly(
      data = data,
      x = ~ DAP,
      y = ~ paste0("variable",type) %>% get(),
      split = ~ genotype,
      type = 'scatter',
      mode = 'lines',
      connectgaps = TRUE)
  }


  # renderPlotly

  output$plot_original   <- renderPlotly({
     newData() %>%  ply_model(., "")
  })

  output$plot_interval   <- renderPlotly({
        plot_ly(
          intervalData(),
          x = ~ DAP_group,
          y = ~ paste0("variable") %>% get(),
          split = ~ genotype,
          type = 'scatter',
          mode = 'lines',
          connectgaps = TRUE)
  })

}



## To be copied in the UI
# mod_SET_analysis_ui("SET_analysis_ui_1")

## To be copied in the server
# callModule(mod_SET_analysis_server, "SET_analysis_ui_1")

