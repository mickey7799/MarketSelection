# Load the ggplot2 package which provides

library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinythemes)

fluidPage(
  theme = shinytheme('united'),
  titlePanel("2018重點市場篩選"),
  
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
      uiOutput('sizeSelector'),
      br(),
      p('群組描述：', align = 'left'),
      p('A: 中低規模、中高成長、中高風險市場', align = 'left'),
      p('B: 中上規模、中高成長、低風險市場', align = 'left'),
      p('C: 小規模、中低成長、高風險市場', align = 'left'),
      p('D: 大規模、中成長、低風險市場', align = 'left'),
      p('E: 極端值或未分群市場', align = 'left')
      
    ), 
    
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
    )
  )
)