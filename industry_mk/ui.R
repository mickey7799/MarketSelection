# Load the ggplot2 package which provides

library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinythemes)

shinyUI(navbarPage("Target",
  theme = shinytheme('united'),
  tabPanel(strong("重點市場篩選"),
           icon =icon('rocket'),
   sidebarLayout(
    # Create a new Row in the UI for selectInputs
    sidebarPanel(
      tags$style(type = 'text/css',
                 # Suppress error messages from reactive functions before output is ready
                 '.shiny-output-error {visibility: hidden;}',
                 '.shiny-output-error:before {visibility: hidden;}'),
      
      h4('選擇市場', align = 'left'),
      uiOutput('groupSelector'),#, style = 'color:#12d5ff'),
      uiOutput('areaSelector'),#, style = 'color:#12d5ff'),
      checkboxGroupInput('macro_criteria', label = h5(strong('選擇市場篩選條件')),#, style = 'color:#12d5ff'),
                         choices = list('具出口規模' = 1, '整體進口正成長' = 2, '臺灣產業具相對優勢度' = 3, 
                                        '自臺進口產品集中度低' = 4, '臺灣進口市占增加大' = 5,
                                        '高出口商家數' = 6, '多中小企業出口商' = 7, '市場風險低' = 8,
                                        '2016年商機總量大於平均' = 9, '2016年觀展公司數大於平均' = 10, 'TT流量大於平均' = 11,
                                        '2016年展覽補助總核准次數大於平均' = 12),  
                         selected = NULL),

 #--------------------------------------------------------------------   
      br(),
      h4('篩選結果指標視覺化', align = 'left'),
      uiOutput('xSelector'),
      uiOutput('ySelector'), 
      uiOutput('sizeSelector')
    ), #sidebarPanel
 
    # Create a new row for the table.
    mainPanel(
      h4('【市場篩選結果】', align = 'center'),
      h4('共有', htmlOutput('wording', inline = T), '個市場符合篩選條件：', align = 'center'),
      DT::dataTableOutput("table"),
      downloadButton('downloadData', 'Download', align='right'),
#------------------------------------------------------------------------      
      #graph
      br(),
      br(),
      h4('篩選結果總表圖示', align = 'center'),
      plotOutput('bubble'),
      
      
      p('資料來源：IMF、Oxford、數據中心演算', align='right'),
      p('2017 版權所有 經濟部國際貿易局主辦 中華民國對外貿易發展協會執行', align='center'),
      #br(),
      p("地址:台北市11012基隆路一段333號5-7樓", align='center'),
      #br(),
      p("電話：(02) 2725-5200", align='center'),
      #br(),
      p("Email: taitra@taitra.org.tw", align='center')
    )#mainPanel
  )#sidebarLayout
),#tabPanel
 tabPanel(strong("產業別選市場"),
          icon =icon('diamond'),
   sidebarLayout(
     sidebarPanel(
       h4('選擇產業', align = 'left'),
       uiOutput('industrySelector'),
      checkboxGroupInput('in_criteria', label = h5(strong('選擇市場篩選條件')),#, style = 'color:#12d5ff'),
                         choices = list('具出口規模(前20名)' = 1, '臺灣出口正成長' = 2, '臺灣出口至該國複合成長率優於至全球' = 3,
                                        '高進口需求(前20名)' = 4, '整體進口正成長' = 5, '台灣進口排名前三' = 6,
                                        '臺灣進口排名上升' = 7, '該國自臺進口成長優於全球' = 8, '進口來源國數小於平均' = 9,
                                        '高出口商數(前20名)' = 10,'前三大出口商占比小於70%' = 11),
                         selected = NULL),
      br(),
      h4('篩選結果指標視覺化', align = 'left'),
      uiOutput('x2Selector'),
      uiOutput('y2Selector'), 
      uiOutput('size2Selector')

      
    ), #sidebarPanel
    mainPanel(
      h4(htmlOutput('wording_in', inline = T), align = 'center'),
      br(),
      br(),
      h4('【市場篩選結果】', align = 'center'),
      h4('共有', htmlOutput('wording2', inline = T), '個市場符合篩選條件：', align = 'center'),
      DT::dataTableOutput("table2"),
      downloadButton('downloadData2', 'Download', align='right'),
      #graph
      br(),
      br(),
      h4('篩選結果圖示', align = 'center'),
      plotOutput('bubble2'),
      
      
      p('資料來源：IMF、Oxford、數據中心演算', align='right'),
      p('2017 版權所有 經濟部國際貿易局主辦 中華民國對外貿易發展協會執行', align='center'),
      #br(),
      p("地址:台北市11012基隆路一段333號5-7樓", align='center'),
      #br(),
      p("電話：(02) 2725-5200", align='center'),
      #br(),
      p("Email: taitra@taitra.org.tw", align='center')
      
    )#mainPanel
  )#sidebarLayout
),#tabPanel
 tabPanel(strong("市場別選產業"),
          icon =icon('trophy'),
         sidebarLayout(
           sidebarPanel(
             h4('選擇市場', align = 'left'),
             uiOutput('marketSelector'),
             checkboxGroupInput('mk_criteria', label = h5(strong('選擇產業篩選條件')),#, style = 'color:#12d5ff'),
                                choices = list('具出口規模(前10名)' = 1, '臺灣出口正成長' = 2, '臺灣出口至該國複合成長率優於至全球' = 3,
                                               '高進口需求(前10名)' = 4, '整體進口正成長' = 5, '高自臺進口需求(前10名)' = 6, '自臺進口正成長' = 7,
                                               '台灣進口排名前三' = 8, '臺灣進口排名上升' = 9, '該國自臺進口成長優於全球' = 10, 
                                               '進口來源國數小於平均' = 11, '高自臺進口比重(前10名)' = 12, '自臺進口比重上升' = 13,
                                               '高出口商數(前10名)' = 14, '高出口商數(中小企業)' = 15, '前三大出口商占比小於70%' = 16),
                                selected = NULL),
             br(),
             h4('篩選結果指標視覺化', align = 'left'),
             uiOutput('x3Selector'),
             uiOutput('y3Selector'), 
             uiOutput('size3Selector')
             
             
           ), #sidebarPanel
           mainPanel(
             h4(htmlOutput('wording_mk', inline = T), align = 'center'),
             br(),
             br(),
             h4('【產業篩選結果】', align = 'center'),
             h4('共有', htmlOutput('wording3', inline = T), '個產業符合篩選條件：', align = 'center'),
             DT::dataTableOutput("table3"),
             downloadButton('downloadData3', 'Download', align='right'),
             #graph
             br(),
             br(),
             h4('篩選結果圖示', align = 'center'),
             plotOutput('bubble3'),
             
             
             p('資料來源：IMF、Oxford、數據中心演算', align='right'),
             p('2017 版權所有 經濟部國際貿易局主辦 中華民國對外貿易發展協會執行', align='center'),
             #br(),
             p("地址:台北市11012基隆路一段333號5-7樓", align='center'),
             #br(),
             p("電話：(02) 2725-5200", align='center'),
             #br(),
             p("Email: taitra@taitra.org.tw", align='center')
             
           )#mainPanel
         )#sidebarLayout
)#tabPanel


)#navbarPage
)#shinyUI