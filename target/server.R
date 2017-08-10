# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)
library(readxl)
library(purrr)

function(input, output) {
  data <- read_excel("data/outputD.xlsx")
  data[,c(14:16,20:21)] <- map(data[,c(14:16,20:21)],as.numeric)
  
  group.list <- c('All', as.character(unique(data$`群組`)) %>% sort())
  output$groupSelector <- renderUI({selectInput('group', '群組:', as.list(group.list))})
  area.list <- c('All', as.character(unique(data$`地區`)) %>% sort())
  output$areaSelector <- renderUI({selectInput('area', '地區:', as.list(area.list))})
  
  data_temp <- reactive({
    
    if (input$group != "All") {
      data <- data[data$`群組` == input$group,]
    }
    if (input$area != "All") {
      data <- data[data$`地區` == input$area,]
    }
    # if (input$hscode != "All") {
    #   data <- data[data$HSCODE == input$hscode,]
    # }
    data
  })
  
  conditions <- reactive({
    list(
      as.vector(scale(data_temp()$`出口額預測2018`)) >= -0.1708,
      data_temp()$`進口成長預測2018` > 0,
      data_temp()$`臺灣產業在該市場的相對優勢度` > 0,
      data_temp()$`各國自臺進口的產品集中度` < 0.1,
      as.vector(scale(data_temp()$`臺灣產業在該市場的進口市占變化`)) >= 0.3614,
      as.vector(scale(data_temp()$`出口商數`)) >= -0.02845,
      as.vector(scale(data_temp()$`中小企業出口商數`)) >= -0.06453,
      data_temp()$`市場風險` %in% c('AA','A'),
      data_temp()$`商機總量2016` > mean(data_temp()$`商機總量2016`),
      data_temp()$`觀展公司數2016` > mean(data_temp()$`觀展公司數2016`),
      data_temp()$`TT流量201609至201706` > mean(data_temp()$`TT流量201609至201706`),
      data_temp()$`展覽補助總核准次數2016` > mean(data_temp()$`展覽補助總核准次數2016`),
      # default condition
      rep(T, nrow(data_temp()))
    )
  })
  
  data_filtered <- reactive({
    
    # Filter table based on selected conditions
    data <- data_temp()[Reduce(`&`, conditions()[as.integer(c(input$macro_criteria, 13))]), ]
    # Remove empty rows
    data[!is.na(data$`國家`), ]
    
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
  output$xSelector <- renderUI({selectInput('x', 'X軸變數:', as.list(x.list), selected='出口成長率預測2018')})  
  y.list <- as.character(names(data)[c(6,8:10,14:16)]) %>% sort()
  output$ySelector <- renderUI({selectInput('y', 'Y軸變數:', as.list(y.list), selected='GDP成長預測2018')})
  size.list <- as.character(names(data)[c(7,11:13,17,22:39)])%>%sort()
  output$sizeSelector <- renderUI({selectInput('size', '泡泡大小變數:', as.list(size.list), selected='出口額預測2018')})  
  
  
  output$bubble <- renderPlot({
    clr <- colorRampPalette(brewer.pal(11, 'Spectral'))
    ggplot(data_filtered(), aes_string(x = input$x, y = input$y, size = input$size , fill = '群組')) +
      geom_point(alpha = 0.6, shape = 21, col = 'grey') +
      geom_text(aes_string(x = input$x, y = input$y, label = '國家'), size = 3) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      ggtitle( paste(as.character(input$x), 'vs', as.character(input$y))) +
      scale_fill_manual('Group', values = clr(6)) +
      scale_size_continuous(range = c(5, 30), guide = "none") +
      theme(plot.title = element_text(size = 24, face = 'bold'),
            axis.title = element_text(size = 16, face = 'bold'),
            axis.text = element_text(face = 'bold')) +
      geom_vline(xintercept = median(data_filtered()[[input$x]]), linetype = 2, col = 'red') +
      geom_hline(yintercept = median(data_filtered()[[input$y]]), linetype = 2, col = 'red')
    
    
  })
  
  
}