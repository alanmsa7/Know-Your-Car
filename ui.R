library(shiny)
library(shinythemes)
library(readxl)
DATAS <- read_excel("NEWDATA.xlsx")
shinyUI(fluidPage(theme=shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(
      selectInput('MODEL', label = strong('Model'), c('Alto800','Swift','Dzire','Wagonr','Celerio','Ritz')),      
      radioButtons('VERSION', label = strong('Variant'), c('LXI','VXI','STD')),
      sliderInput('DISPLACEMENT', label = strong('Displacement'), min = 1000, max = max(DATAS$DISPLACEMENT)+10000, step = 100, value = 50000),
      sliderInput('AGE', label = strong('Age'), min= min(DATAS$AGE), max=2019, step = 1, value =2014),
      selectInput('OWNERSHIP',label = strong('Owners'), choices = c(1,2,3,4,5)),
      radioButtons('TRANSMISSION', label = strong('Transmission Type'), c('Manual')),
      actionButton('PREDICTPRICE', label = strong('Predict Price',class = "btn btn-outline-success" )
                )
              ),
     
  mainPanel(
    div(tags$img(src='CAR.png', height=175,width=225), style="text-align: left;"),
    hr(),
    h6("Know the real worth of your car"),
    hr(),
    
    
    tabsetPanel(
      tabPanel("Prediction", 
               fluidRow(
                 tags$br(),
                 column(4,
                        strong("Predicted Price    :    Rs."),
                        strong(verbatimTextOutput("prediction"))
                        )
                      )
                  ),
      
      tabPanel('Plot',                     
               fluidRow(
                 hr(),
                 h6("Factors that affect the  Price"),
                 plotOutput("Plot")
                       ) 
              ),
      tabPanel('Info',
              tags$br(),
                p(strong("Know your Car"),"focuses on predicting the price of Maruti Suzuki made cars."),
                p("After a few statistical tests, the following variables seemed to be significant in predicting the price of a car."),
               tags$br(),
               tags$ul(
                 tags$li("Displacement    : Distance  in km the car has run"),
                 tags$li("Age             : Year of first registration"),
                 tags$li("Model           : Brand name of the car"),
                 tags$li("Variant         : Variants of the car model"),
                 tags$li("Ownership       : Number of previous users"),
                 tags$li("Transmission    : Automatic or Manual transmission")
               )
               ),
      
      tabPanel("About",
               hr(),
               fluidRow(
                 column(4,
                        h5("Developed by,"),
                        h5("Alan M Shaji"),
                        tags$a("alan.naka1996.com"),
                        h5("Jayakrishnan KS"),
                        tags$a("jayakrishred0000.com")       
                 )
               ),
               h5(""),
               fluidRow(
                 column(4,
                        h5("Guided by,"),                     
                        h5("Prof.Neetha K Nataraj"),
                        h6("CSE Department"),
                        h6("Adi Shankara Institute of Engineering and Technology, Kalady")
                        )),
                        hr()
      )
    )
              )
      )
    )
  )
