#-----------------------------------------------#
# shinyServer session #
#-----------------------------------------------#

shinyServer(function(input, output, session) {
  
  Bio_ply_out <- reactive({
    Pepper.PhenoData1 <- Pepper.PhenoData[ Pepper.PhenoData$timestamp >= input$timeRange[1]
                                           & Pepper.PhenoData$timestamp <= input$timeRange[2],]
    plyDataformt(Pepper.PhenoData1, 1, input$pheno.parm) })
  
  Bio_ply_out_adj.raw <- reactive({
    Pepper.PhenoData.adj <- Pepper.PhenoData.adj.block
    Pepper.PhenoData.adj <- Pepper.PhenoData.adj.block[ Pepper.PhenoData.adj.block$Date >= (input$timeRange[1] %>% as.Date())
                                                        & Pepper.PhenoData.adj.block$Date <= (input$timeRange[2] %>% as.Date() ),]
  })
  
  
  hmp_ply_out <- reactive({
    Pepper.PhenoData1 <- Pepper.PhenoData[ Pepper.PhenoData$timestamp >= input$timeRange[1]
                                           & Pepper.PhenoData$timestamp <= input$timeRange[2],]
    Bio_ply_tile(Pepper.PhenoData1, input$pheno.parm)
  })
  
  hmp_ply_out2 <- reactive({
    Pepper.PhenoData.adj2 <- ply_block_adj_all_for_hmp
    Pepper.PhenoData.adj2 <- Pepper.PhenoData.adj2[ Pepper.PhenoData.adj2$Date >= (input$timeRange[1] %>% as.Date())
                                                    & Pepper.PhenoData.adj2$Date <= (input$timeRange[2] %>% as.Date() ),]
    Bio_ply_tile(Pepper.PhenoData.adj2, input$pheno.parm)
  })
  
  
  Bio_ply_out_adj <- reactive({
    Pepper.PhenoData.adj <- Pepper.PhenoData.adj.block
    Pepper.PhenoData.adj <- Pepper.PhenoData.adj[ Pepper.PhenoData.adj$Date >= (input$timeRange[1] %>% as.Date())
                                                  & Pepper.PhenoData.adj$Date <= (input$timeRange[2] %>% as.Date() ),]
    data.date.smoothed <- date.smooth(Pepper.PhenoData.adj, input$pheno.parm, input$smooth.parm %>% as.integer(), 1:5, treatment, DAP)
    model.smoothed(data.date.smoothed, input$pheno.parm, input$smooth.method, input$smooth.df %>% as.integer(), "treatment", c("xDAP","DAP", "Date"), "DAP_group")
  })
  
  
  
  output$distPlot1 <- renderPlotly({
    plot_ly(
      data = Bio_ply_out(),
      x = ~allDates,
      y = ~value,
      split = ~variable,
      type = 'scatter',
      mode = 'lines') %>%
      layout(#title = "without block adjustment", 
        xaxis = list(title = "Date"), 
        yaxis = list(title = input$pheno.parm))
  }) #end output$distPlot
  
  output$distPlot2 <- renderPlotly({
    plot_ly(
      data = Bio_ply_out_adj.raw(),
      x = ~Date,
      y = ~input$pheno.parm %>% get(),
      split = ~treatment,
      type = 'scatter',
      mode = 'lines') %>%
      layout(#title = "with block adjustment", 
        xaxis = list(title = "Date"), 
        yaxis = list(title = input$pheno.parm))
  }) #end output$distPlot
  
  output$hmpPlot1 <- renderPlotly({
    plot_ly(
      data = hmp_ply_out(),
      x = hmp_ply_out()[,1]%>% as.numeric(),
      y = hmp_ply_out()[,2]%>% as.numeric(),
      z = hmp_ply_out()[,4],
      text = hmp_ply_out()$編號,
      type = "heatmap",
      colors="YlGnBu") %>%
      add_annotations(
        x = hmp_ply_out()$Col %>% as.numeric(),
        y = hmp_ply_out()$Pos %>% as.numeric(),
        text = hmp_ply_out()$編號,
        showarrow = FALSE,
        ax = 20,
        ay = -20)
  }) # end renderPlotly
  
  output$hmpPlot2 <- renderPlotly({
    plot_ly(
      data = hmp_ply_out2(),
      x = hmp_ply_out2()[,1]%>% as.numeric(),
      y = hmp_ply_out2()[,2]%>% as.numeric(),
      z = hmp_ply_out2()[,4],
      text = hmp_ply_out2()$編號,
      type = "heatmap",
      colors="YlGnBu") %>%
      add_annotations(
        x = hmp_ply_out2()$Col %>% as.numeric(),
        y = hmp_ply_out2()$Pos %>% as.numeric(),
        text = hmp_ply_out2()$編號,
        showarrow = FALSE,
        ax = 20,
        ay = -20)
  }) # end renderPlotly
  
  output$distPlot.response.tab   <-  output$distPlot.response   <- renderPlotly({
    ply_model(Bio_ply_out_adj(), input$pheno.parm, "",            "DAP_group", "treatment", 
              paste("smooth day:", input$smooth.parm ,", method:", input$smooth.method, ", df:" ,input$smooth.df)) }) #end distPlot.response
  
  output$distPlot.smooth.tab     <-  output$distPlot.smooth     <- renderPlotly({
    ply_model(Bio_ply_out_adj(), input$pheno.parm, ".smooth",     "DAP_group", "treatment",
              paste("smooth day:", input$smooth.parm ,", method:", input$smooth.method, ", df:" ,input$smooth.df)) }) #end distPlot.smooth
  
  output$distPlot.AGR.tab        <- output$distPlot.AGR        <- renderPlotly({
    ply_model(Bio_ply_out_adj(), input$pheno.parm, ".AGR",        "DAP_group", "treatment",
              paste("smooth day:", input$smooth.parm ,", method:", input$smooth.method, ", df:" ,input$smooth.df)) }) #end distPlot.AGR
  
  output$distPlot.RGR.tab        <- output$distPlot.RGR        <- renderPlotly({
    ply_model(Bio_ply_out_adj(), input$pheno.parm, ".RGR",        "DAP_group", "treatment",
              paste("smooth day:", input$smooth.parm ,", method:", input$smooth.method, ", df:" ,input$smooth.df)) }) #end distPlot.RGR
  
  output$distPlot.AGR.smooth.tab <- output$distPlot.AGR.smooth <- renderPlotly({
    ply_model(Bio_ply_out_adj(), input$pheno.parm, ".AGR.smooth", "DAP_group", "treatment",
              paste("smooth day:", input$smooth.parm ,", method:", input$smooth.method, ", df:" ,input$smooth.df)) }) #end distPlot.AGR.smooth
  
  output$distPlot.RGR.smooth.tab <- output$distPlot.RGR.smooth <- renderPlotly({
    ply_model(Bio_ply_out_adj(), input$pheno.parm, ".RGR.smooth", "DAP_group", "treatment",
              paste("smooth day:", input$smooth.parm ,", method:", input$smooth.method, ", df:" ,input$smooth.df)) }) #end distPlot.RGR.smooth
  
  output$median.dev.plot.response.tab <-   output$median.dev.plot.response <- renderPlot({
    median.dev.plot(Bio_ply_out_adj() , input$pheno.parm, "response", c(4,6,12), "DAP_group", "treatment") }) #end median.dev.plot.response
  
  output$median.dev.plot.AGR.tab  <-  output$median.dev.plot.AGR      <- renderPlot({
    median.dev.plot(Bio_ply_out_adj() , input$pheno.parm, "AGR",      c(4,6,12), "DAP_group", "treatment") }) #end median.dev.plot.response
  
  output$median.dev.plot.RGR.tab      <- output$median.dev.plot.RGR  <- renderPlot({
    median.dev.plot(Bio_ply_out_adj() , input$pheno.parm, "RGR",      c(4,6,12), "DAP_group", "treatment") }) #end median.dev.plot.response
}) #end server
