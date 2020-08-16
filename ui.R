#Author: Zaporozhtsev I.F.
#Created: May, 2020

#### Libraries ####

library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(shinyBS)
library(htmlwidgets)
library(DT)
 
# library(plotly)
#### end Libraries ####

shinyUI(fluidPage(useShinyjs(),useShinyalert(),
  setBackgroundColor(
    color = c("#AFCDE7", "#081A66"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel("Калькулятор норм и аномалий на разрезе \"Кольский меридиан\"", windowTitle = "MMBI_NORMANO_KT2020"),
  includeCSS('./www/styles.css'),

sidebarLayout(fluid = TRUE,
sidebarPanel(width = 3,
#### INI sidebarPanel ####
 tags$div(id="ANA_SIDEPANELBLOCKST",
          #tags$style(".shiny-file-input-progress {display: none}"),
           h4("Настройка норм"),
          
          fluidRow(column(12, radioButtons("norm_charactRB", label = "Выберите характеристику",
                       choices = list("температура", "солёность"), 
                       selected = "температура", inline=T))),
           fluidRow(column(12, strong("Выберите период"))),
           fluidRow(column(12, div(style="display: inline-block; width:10px; ",p("c")),
                          div(style="display: inline-block; width:80px; ", selectInput(inputId="norm_year_fromSI",
                                                                        label = NULL,choices = seq(1970,2020,by=1), selected = "1970")),
                          div(style="display: inline-block; width:20px; ",p("по")),
                          div(style="display: inline-block; width:80px; margin-top:10px;", selectInput(inputId="norm_year_toSI",
                                                                        label = NULL,choices = seq(1970,2020,by=1), selected = "2020")))),
            verticalLayout(
              div(style="margin:5px;",fluidRow(column(12, downloadButton("norm_exportB","Экспорт в MS Excel (WOD)", width="100%")))),
              shinyjs::hidden(div(id="norm_exportP_Panel",style="margin:5px;",
                                  fluidRow(column(12, progressBar(id = "norm_exportP", value = 0, 
                                                                  display_pct = T,title = "Идёт подготовка к экспорту. Ожидайте..."))))),
              div(style="margin:5px;",fluidRow(column(12, downloadButton("norm_exportallB","Экспорт в MS Excel (всё)", width="100%")))),
            
              
              div(style="margin:5px;",fluidRow(column(12,
                      fileInput("norm_supplyFI","Пополнить БД норм", width="100%",buttonLabel = "Обзор",placeholder = "Файл не выбран",accept = ".xlsx", multiple = TRUE)
                ))),
                shinyjs::hidden(div(id="norm_supplyP_Panel",style="margin:5px;",
                                  fluidRow(column(12, progressBar(id = "norm_supplyP", value = 0, 
                                                                  display_pct = T,title = "Выполняется обновление БД. Ожидайте..."))))),
              
              div(style="margin:5px;",fluidRow(column(12,
                                                      actionButton("norm_clearB","!ОЧИСТИТЬ ДОПЫ!", width="100%")
              ))),
              
              h4("Состав БД норм:",align = "center"),
              p("I.  WOD v2018 (upd 2019)"),
              p("II. Данные лаборатории ОиР ММБИ:"),
              DTOutput("norm_listexpDT"),
              div(img(src="mmbi.png",width="50%",height="50%"), style="margin:10px; text-align: center;")
            )
          
)
#### END sidebarPanel ####
),

#######################
# INI mainPanel
#######################
mainPanel(width = 9,
          tags$style(HTML("
                  .tabbable > .nav > li > a                  {font-weight: bold; background-color: #F5F5F5;  color:black}
                  .tabbable > .nav > li[class=active]    > a {background-color: gold; color:black}
                  ")),
            tags$style(HTML("hr {border-top: 1px solid #000000;}")),

    tabsetPanel(id = "tabset",
       tabPanel("Построение аномалий",value="1",
               column(12,style="background-color:#D3E4F3",
                      
              br(),
              
              fluidRow(column(12,div(style="display: inline-block; width:250px; margin-top:8px",strong("Выберите референсный месяц:")),
                div(style="display: inline-block; width:200px; vertical-align:top;", selectInput(inputId="ano_monSI",label = NULL,
                        choices = c("январь","февраль","март","апрель","май","июнь","июль",
                                    "август","сентябрь","октябрь","ноябрь","декабрь"), selected = "январь"))
                )),
              br(),div(style="height: 2px; border: 1px solid black;"),br(),
              strong("Для всех экспедиций: только для референсного месяца и с группировокой экспедиций внутри месяца каждого года"),
              br(),br(),
              fluidRow(column(12,downloadButton("ano_allWODanoTestB","Рассчитать аномалии и экспортировать в MS Excel"))),
              
              br(),div(style="height: 2px; border: 1px solid black;"),br(),
              
              strong("Для одной экспедиции: все данные из файла будут приведены к референсному месяцу"),
              br(),br(),
              fluidRow(column(12,fileInput("ano_xlsFI","Загрузите данные экспедиции (.xlsx)",buttonLabel = "Обзор",placeholder = "Файл не выбран",accept = ".xlsx", multiple = FALSE))),

              fluidRow(column(12,downloadButton("ano_runandexportB","Рассчитать аномалии и экспортировать в MS Excel"))),
            
              br(), br(),br(),br(),  br(),br(),  br(),  br(),  br(),  br(),   br(),   br(),    br(),  br()
      )),
  
     
   tabPanel("О программе",value="2",column(12, style="background-color:#D3E4F3",
                                           column(8,includeMarkdown("./www/helpabout.md")),column(4,tags$img(src="mmbi.png",height="100%",width="100%")))
    )
  )                


#######################
# END mainPanel
#######################
) #mainPanel   
) #sidebarLayout closed
#) #tabpanel of navbar
#) #navbar
))