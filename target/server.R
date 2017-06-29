# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)


function(input, output) {
  data <- read_csv("data/outputB.csv")
  
  group.list <- c('All', as.character(unique(data$label)) %>% sort())
  output$groupSelector <- renderUI({selectInput('group', '群組:', as.list(group.list))})
  area.list <- c('All', as.character(unique(data$region)) %>% sort())
  output$areaSelector <- renderUI({selectInput('area', '地區:', as.list(area.list))})
  
  data_temp <- reactive({
    
    if (input$group != "All") {
      data <- data[data$label == input$group,]
    }
    if (input$area != "All") {
      data <- data[data$region == input$area,]
    }
    # if (input$hscode != "All") {
    #   data <- data[data$HSCODE == input$hscode,]
    # }
    data
  })
  
  conditions <- reactive({
    list(
      as.vector(scale(data_temp()$export.pred)) >= -0.1907,
      data_temp()$im_growth > 0,
      data_temp()$nrca.ttl > 0,
      data_temp()$herf < 0.1,
      as.vector(scale(data_temp()$share.ttl)) >= 0.3614,
      as.vector(scale(data_temp()$company_n)) >= -0.02845,
      as.vector(scale(data_temp()$company_n8000)) >= -0.06453,
      data_temp()$risk %in% c('AA','A'),
      # default condition
      rep(T, nrow(data_temp()))
    )
  })
  
  data_filtered <- reactive({
    
    # Filter table based on selected conditions
    data <- data_temp()[Reduce(`&`, conditions()[as.integer(c(input$macro_criteria, 9))]), ]
    # Remove empty rows
    data[!is.na(data$reporter), ]
    
  })
  
  #Add wording
  output$wording <- renderText({
    paste0('<font color=\"#D73027\"><b>', nrow(data_filtered()), '</b></font>')
    
  })
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data_filtered()
  }))
  
  #download file
  output$downloadData <- downloadHandler(
    filename = 'targetmarket.csv',
    content = function(file) {
      write.csv(data_filtered(), file)
    }
  )
#-----------------------------------------------------------------------  
  
  x.list <- as.character(names(data)[c(6,8:10,14:16)]) %>% sort()
  output$xSelector <- renderUI({selectInput('x', 'X軸變數:', as.list(x.list), selected='grate.pred')})  
  y.list <- as.character(names(data)[c(6,8:10,14:16)]) %>% sort()
  output$ySelector <- renderUI({selectInput('y', 'Y軸變數:', as.list(y.list), selected='gdp_growth')})
  size.list <- as.character(names(data)[c(7,11:13,17)])%>%sort()
  output$sizeSelector <- renderUI({selectInput('size', '泡泡大小變數:', as.list(size.list), selected='export.pred')})  
  
  #filter data for plotting
  data_plot <- reactive({
    
    data <- data_filtered()[, c(input$x, input$y, input$size)]
    #data_filtered() <- data.frame(lapply(data_filtered()[,c(4:8,11:13,15:16)], function(x) {(x - min(x))/(max(x) - min(x))}))
    data
  })
  
  #output$table2 <- DT::renderDataTable(DT::datatable({
  #  data_plot()
  #}))
  
  output$bubble <- renderPlot({
    clr <- colorRampPalette(brewer.pal(11, 'Spectral'))
    ggplot(data_filtered(), aes_string(x = input$x, y = input$y, size = input$size , fill = 'label')) +
      geom_point(alpha = 0.6, shape = 21, col = 'grey') +
      geom_text(aes_string(x = input$x, y = input$y, label = 'reporter'), size = 3) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      ggtitle( paste(as.character(input$x), 'vs', as.character(input$y))) +
      scale_fill_manual('Group', values = clr(6)) +
      scale_size_continuous(range = c(5, 30), guide = "none") +
      theme(plot.title = element_text(size = 24, face = 'bold'),
            axis.title = element_text(size = 16, face = 'bold'),
            axis.text = element_text(face = 'bold')) +
      geom_vline(xintercept = 3.6, linetype = 2, col = 'red') +
      geom_hline(yintercept = 5.08, linetype = 2, col = 'red')
    
  })
  
  
}