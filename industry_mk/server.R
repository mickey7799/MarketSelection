# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(tidyverse)
#library(ggplot2)
#library(dplyr)
#library(readr)
library(RColorBrewer)
library(scales)

function(input, output) {
  data <- read_excel("data/outputD.xlsx")
  data[,c(14:16,20:21)] <- map(data[,c(14:16,20:21)],as.numeric)
  
  data2 <- read_csv("data/industry.csv",locale=locale(encoding = 'Big5'))
  data2[,c(6:7,12:13)] <- map(data2[,c(6:7,12:13)],as.numeric)
  
  data3 <- read_csv("data/industry_mk.csv", locale=locale(encoding = 'Big5'))
  
  
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
      geom_vline(xintercept = median(data[[input$x]], na.rm=T), linetype = 2, col = 'red') +
      geom_hline(yintercept = median(data[[input$y]], na.rm=T), linetype = 2, col = 'red')
    
  })
  
  
###########################################################################  
#     industry  
###########################################################################
  
  
  industry.list <- gsub('^0', '', sort(gsub('(^\\d{1}[.].*)', '0\\1', unique(data2$industry))))
  output$industrySelector <- renderUI({selectInput('industry', '產業:', as.list(industry.list))})
  
   data2_temp <- reactive({
   
       data2 <- data2[data2$industry == input$industry,]
     
     
     data2
   })
  

  
 conditions2 <- reactive({
   list(
     data2_temp()$`出口額2016` >= arrange(data2_temp(), desc(`出口額2016`))[[20,'出口額2016']],
     data2_temp()$`出口複合成長率2013_2016` > 0,
     data2_temp()$`出口他國成長率是否優於全球` =='Y',
     data2_temp()$`該國自全球進口總額` >= arrange(data2_temp(), desc(`該國自全球進口總額`))[[20,'該國自全球進口總額']],
     data2_temp()$`該國自全球進口複合成長2013_2016` > 0,
     data2_temp()$`臺灣2016年進口排名` < 4,
     data2_temp()$`排名是否上升` =='Y',
     data2_temp()$`該國自臺進口成長是否優於全球` == 'Y',
     data2_temp()$`進口來源國數` < mean(data2_temp()$`進口來源國數`),
     data2_temp()$`出口商數`>= arrange(data2_temp(), desc(`出口商數`))[[20,'出口商數']],
     data2_temp()$`前三大出口商占比`< 70,
     # default condition
     rep(T, nrow(data2_temp()))
   )
 })

 data2_filtered <- reactive({

   # Filter table based on selected conditions
   data <- data2_temp()[Reduce(`&`, conditions2()[as.integer(c(input$in_criteria, 12))]), ]
   # Remove empty rows
   data[!is.na(data$國家), ]
 
 })

#Add wording
output$wording2 <- renderText({
  paste0('<font color=\"#D73027\"><b>', nrow(data2_filtered()), '</b></font>')

})

output$wording_in <- renderText({
  paste0('<font color=\"#3399ff\"><b>', '[',input$industry, ']','</b></font>')
  
})

#Filter data based on selections
output$table2 <- DT::renderDataTable(DT::datatable({
  data2_filtered()
}))
 

#download file
output$downloadData2 <- downloadHandler(
  filename = paste0(input$industry ,'.csv'),
  content = function(file) {
    write.csv(data2_filtered(), file)
  }
)

#######plot######################################################################

  x2.list <- as.character(names(data2)[c(3,15,16)]) %>% sort()
  output$x2Selector <- renderUI({selectInput('x2', 'X軸變數:', as.list(x2.list), selected='複合成長率2013_2016')})  
  y2.list <- as.character(names(data2)[c(2,4,9:11,18:21)]) %>% sort()
  output$y2Selector <- renderUI({selectInput('y2', 'Y軸變數:', as.list(y2.list), selected='臺灣出口比重2016')})
  size2.list <- as.character(names(data2)[c(2,9:10,18:20)])%>%sort()
  output$size2Selector <- renderUI({selectInput('size2', '泡泡大小變數:', as.list(size2.list), selected='出口額2016')})  

  
  output$bubble2 <- renderPlot({

    clr <- colorRampPalette(brewer.pal(11, 'Spectral'))
    
    tn <- trans_new("logpeps",
                    function(x) ifelse(x<0, -1, 1) * log10(abs(x)),
                    function(y) ifelse(y<0, -1, 1) * 10^abs(y),
                    domain=c(-Inf, Inf))
    
    brks <- seq(-4, 4, 1)
        
    ggplot(data2_filtered(), aes_string(x = input$x2, y = input$y2, size = input$size2, fill = '排名是否上升')) +
      geom_point(alpha = 0.6, shape = 21, col = 'grey') +
      geom_text(aes_string(x = input$x2, y = input$y2, label = '國家'), size = 3) +
      #geom_vline(xintercept = 1, col = 'blue') +
      #geom_hline(yintercept = 1, col = 'blue') +
      ggtitle( paste(as.character(input$x2), 'vs', as.character(input$y2))) +
      scale_fill_manual('進口排名是否上升', values = clr(6)) +
      scale_size_continuous(range = c(5, 30), guide = "none") +
      # scale_x_log10(name = paste0('log(',input$x2,')'))+
      scale_y_log10(name = paste0('log(',input$y2,')'))+
      theme(plot.title = element_text(size = 24, face = 'bold'),
            axis.title = element_text(size = 16, face = 'bold'),
            axis.text = element_text(face = 'bold')) +
      geom_vline(xintercept = median(data2_filtered()[[input$x2]], na.rm=T), linetype = 2, col = 'red') +
      geom_hline(yintercept = median(data2_filtered()[[input$y2]], na.rm=T), linetype = 2, col = 'red') +
      coord_trans(x = tn) +
      scale_x_continuous(paste0('log( | ',input$x2,' | )'),breaks = 10 ^ abs(brks) * c(rep(-1, length(which(brks<0))),
                                                     rep(1, length(which(brks>=0)))),
                         labels = as.character(brks))
    
    
  })

  
  
  
  ###########################################################################  
  #     industry_mk  
  ###########################################################################
  
  market.list <- as.character(unique(data3$`國家`) %>% sort())
  output$marketSelector <- renderUI({selectInput('industry_mk', '國家:', as.list(market.list), selected='中國大陸')})
  
  data3_temp <- reactive({
    
    data3 <- data3[data3$國家 == input$industry_mk,]
    
    
    data3
  })
  
  
  
  conditions3 <- reactive({
    list(
      data3_temp()$`臺灣出口至該國總額2016` >= arrange(data3_temp(), desc(`臺灣出口至該國總額2016`))[[10,'臺灣出口至該國總額2016']],
      data3_temp()$`出口至該國複合成長率2013_2016` > 0,
      data3_temp()$`臺灣出口該國複合成長率是否優於至全球` =='Y',
      data3_temp()$`該國自全球進口總額2016` >= arrange(data3_temp(), desc(`該國自全球進口總額2016`))[[10,'該國自全球進口總額2016']],
      data3_temp()$`該國自全球進口複合成長率2013_2016` > 0,
      data3_temp()$`該國自臺灣進口總額2016` >= arrange(data3_temp(), desc(`該國自臺灣進口總額2016`))[[10,'該國自臺灣進口總額2016']],
      data3_temp()$`該國自臺進口複合成長率2013_2016` > 0,
      data3_temp()$`該國對臺灣進口排名2016` < 4,
      data3_temp()$`進口排名是否上升` =='Y',
      data3_temp()$`該國自臺進口複合成長率是否優於自全球2013_2016` == 'Y',
      data3_temp()$`進口來源國數` < mean(data3_temp()$`進口來源國數`),
      data3_temp()$`該國自臺進口比重2016` >= arrange(data3_temp(), desc(`該國自臺進口比重2016`))[[10,'該國自臺進口比重2016']],
      data3_temp()$`該國自臺進口比重是否上升2016` =='Y',
      data3_temp()$`臺灣出口至該國廠商家數`>= arrange(data3_temp(), desc(`臺灣出口至該國廠商家數`))[[10,'臺灣出口至該國廠商家數']],
      data3_temp()$`臺灣出口至該國中小企業家數`>= arrange(data3_temp(), desc(`臺灣出口至該國中小企業家數`))[[10,'臺灣出口至該國中小企業家數']],
      data3_temp()$`前3大出口廠商對該國出口總金額占比`< 70,
      # default condition
      rep(T, nrow(data3_temp()))
    )
  })
  
  data3_filtered <- reactive({
    
    # Filter table based on selected conditions
    data <- data3_temp()[Reduce(`&`, conditions3()[as.integer(c(input$mk_criteria, 17))]), ]
    # Remove empty rows
    data[!is.na(data$產業), ]
    
  })
  
  #Add wording
  output$wording3 <- renderText({
    paste0('<font color=\"#D73027\"><b>', nrow(data3_filtered()), '</b></font>')
    
  })
  
  output$wording_mk <- renderText({
    paste0('<font color=\"#3399ff\"><b>', '[',input$industry_mk, ']','</b></font>')
    
  })
  
  #Filter data based on selections
  output$table3 <- DT::renderDataTable(DT::datatable({
    data3_filtered()
  }))
 
  #download file
  output$downloadData3 <- downloadHandler(
    filename = paste0(input$industry_mk ,'.csv'),
    content = function(file) {
      write.csv(data3_filtered(), file)
    }
  ) 
  
  #######plot######################################################################
  
  x3.list <- as.character(names(data3)[c(9,10,21,22)]) %>% sort()
  output$x3Selector <- renderUI({selectInput('x3', 'X軸變數:', as.list(x3.list), selected='出口至該國複合成長率2013_2016')})  
  y3.list <- as.character(names(data3)[c(3:6,8,12:14,17:18)]) %>% sort()
  output$y3Selector <- renderUI({selectInput('y3', 'Y軸變數:', as.list(y3.list), selected='該國自臺進口比重2016')})
  size3.list <- as.character(names(data3)[c(3,4,12:14,20,24:26)])%>%sort()
  output$size3Selector <- renderUI({selectInput('size3', '泡泡大小變數:', as.list(size3.list), selected='臺灣出口至該國總額2016')})  
  
  
  output$bubble3 <- renderPlot({
    
    clr <- colorRampPalette(brewer.pal(11, 'Spectral'))
    
    tn <- trans_new("logpeps",
                    function(x) ifelse(x<0, -1, 1) * log10(abs(x)),
                    function(y) ifelse(y<0, -1, 1) * 10^abs(y),
                    domain=c(-Inf, Inf))
    
    brks <- seq(-4, 4, 1)
    
    ggplot(data3_filtered(), aes_string(x = input$x3, y = input$y3, size = input$size3, fill = '進口排名是否上升')) +
      geom_point(alpha = 0.6, shape = 21, col = 'grey') +
      geom_text(aes_string(x = input$x3, y = input$y3, label = '產業'), size = 3) +
      #geom_vline(xintercept = 1, col = 'blue') +
      #geom_hline(yintercept = 1, col = 'blue') +
      ggtitle( paste(as.character(input$x3), 'vs', as.character(input$y3))) +
      scale_fill_manual('進口排名是否上升', values = clr(3)) +
      scale_size_continuous(range = c(5, 30), guide = "none") +
      # scale_x_log10(name = paste0('log(',input$x2,')'))+
      scale_y_log10(name = paste0('log(',input$y3,')'))+
      theme(plot.title = element_text(size = 24, face = 'bold'),
            axis.title = element_text(size = 16, face = 'bold'),
            axis.text = element_text(face = 'bold')) +
      geom_vline(xintercept = median(data3_filtered()[[input$x3]], na.rm=T), linetype = 2, col = 'red') +
      geom_hline(yintercept = median(data3_filtered()[[input$y3]], na.rm=T), linetype = 2, col = 'red') #+
      #coord_trans(x = tn) +
      #scale_x_continuous(paste0('log( | ',input$x3,' | )'),breaks = 10 ^ abs(brks) * c(rep(-1, length(which(brks<0))),
      #                                                                                 rep(1, length(which(brks>=0)))),
      #                   labels = as.character(brks))
    
    
  })
  
  
  
  
  
  
}