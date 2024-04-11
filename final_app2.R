
library(forecast)
install.packages("shiny")

library(shiny)

library(DT)
ui <- fluidPage(
        # App title ----
        titlePanel("RAM collecting waste tool"),
        h5("Author: Essounaini Soufiane"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        
                        # Input: Select a file ----
                        fileInput("upload_file", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: Select a column ----
                        selectInput("ohlcv", "Select a column:",
                                    choices = c('select','Scrap','date'),
                                    selected = 'select'),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: Select a graph ----
                        selectInput("graphs", "Select a graph:",
                                    choices = c('Select','TimeSeries','DecomposedSeries','DifferencedSeries'),
                                    selected = 'Select'),
                        
                        
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: ARIMA ----
                        selectInput("arima", "Select a model:",
                                    choices = c('Select','ARIMA',"Holt's method","Exponential smoothing"),
                                    selected = 'Select'),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: Enter number of weekss value ----
                        numericInput("nweeks", "Number of weeks to forecast:", value=0),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: Forecast ----
                        selectInput("forecast", "Select to Forecast scrap generated in the upcoming weeks:",
                                    choices = c('Select','Forecast-Plot','Forecast-Values'), 
                                    selected = 'Select')
                        
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(h3(textOutput("caption")),
                          
                          tabsetPanel(
                            tabPanel("Main",tableOutput("dataTable"),
                                     plotOutput("graphs"),
                                     verbatimTextOutput('arimaPlot'),
                                     plotOutput("forecastPlot"),
                                     tableOutput('forecastTable')),
                            tabPanel("Accuracy Measures for arima model", dataTableOutput("selection")),
                            tabPanel("Accuracy measures for Holt method",dataTableOutput("selection1")),
                            tabPanel("Accuracy measures for Simple exponential smoothing method",dataTableOutput("selection2")))
                          ))
                
        )
      

# Wrap your UI with secure_app



server <- function(input, output, session) {
        
        # call the server part
        # check_credentials returns a function to authenticate users
        
        
        # your classic server logic
        getContents <- reactive({
                # input$upload_file will be NULL initially. After the user selects and uploads a file
                req(input$upload_file)
                uf <<- read.csv(input$upload_file$datapath)
                uf[is.na(uf)] <- 0
                uf$date <- as.Date(uf$date, format = "%Y-%m-%d")
                return(uf)
        })
        
        output$dataTable <- renderTable({		
                head(getContents())
        })
        
        output$graphs <- renderPlot({
                its <<- create_ts(which(colnames(getContents()) == getColOpt(input$ohlcv)))
                actualSeries <<- getContents()[,which(colnames(getContents()) == getColOpt(input$ohlcv))]
                differncedSeries <<- diff(its, differences=1)
                
                if(input$graphs == 'TimeSeries'){
                        plot.ts(its, main = "Time-Series plot", col = "blue")
                } else if(input$graphs == 'DecomposedSeries'){
                        stldecomp = stl(its, s.window="periodic")
                        plot(stldecomp)
                } else if(input$graphs == 'DifferencedSeries'){
                        plot.ts(differncedSeries,col = "blue")
                }
        })
        
        
        
        output$arimaPlot <- renderText({ 
                if(input$arima == 'ARIMA'){
                        amv <<- arimafunc()
                        paste("AIC:",amv$aic," AICc:",amv$aicc," BIC:",amv$bic,"residuals:",amv$residuals)
                }else if (input$arima == "Holt's method"){
                        amv<<- Holtfunc()
                } else {
                        amv<<- Expfunc()
                }
        
        })
        
        output$forecastPlot <- renderPlot({
                if(input$forecast == 'Forecast-Plot'){
                        if(input$nweeks == 0){
                                error()
                        } else {
                                fc <<- forecastfunc()
                                plot(fc, col = "darkgreen")
                        }
                }
        })
        
        output$forecastTable <- renderTable({
                if(input$forecast == 'Forecast-Values'){
                        if(input$nweeks == 0){
                                error()
                        } else {
                                yy <- fc$mean
                                cd <- data.frame(ForecastedWaste = c(yy))
                                return(cd)
                        }
                }
        })
        
        error <- function(){
                print("enter valid input")
        }
        
        
        output$value <- renderText({ input$nweeks })
        
        getColOpt <- function(cv){
                if(cv == 'select'){
                        print("Choose a column")
                } else if(cv == 'Scrap'){
                        return("Scrap")
                } else if(cv == 'date'){
                        return("date")
                }
        }
        
        
        
        create_ts <- function(col_idx){
                ## Create a time series object
                if (input$ohlcv == 'select'){
                        print("select please")
                } else {
                        i_ts <- as.numeric(getContents()[,col_idx]) %>%
                                tsclean(replace.missing = TRUE, lambda = NULL) %>%
                                ts(start = 2021,
                                   frequency = 52)
                        return(i_ts)
                }
        }
        
        
        
        arimafunc <- function(){
          if(input$arima == "ARIMA"){
            M_arima <- auto.arima(actualSeries)
            return(M_arima)
            
          }
        }
        Holtfunc <- function(){
          if(input$arima == "Holt's method"){
            M_holt <- HoltWinters(actualSeries,gamma=FALSE)
            return(M_holt)
              
          }
        }
        Expfunc <- function(){
          if(input$arima == "Exponential smoothing"){
            M_exp <- HoltWinters(actualSeries,beta=FALSE,gamma=FALSE)
            return(M_exp)
          }
        }
        output$selection <- DT::renderDataTable({
          
          fit <- auto.arima(actualSeries)%>%
            forecast(h= input$nweeks)
          
          selection <- fit %>% accuracy()
          
          selection
          
        })
        output$selection1 <- DT::renderDataTable({
          fit <- HoltWinters(actualSeries,gamma=FALSE)%>%
            forecast(h= input$nweeks)
          
          selection1 <- fit %>% accuracy()
          
          selection1
          
        })
        output$selection2 <- DT::renderDataTable({
          fit <- HoltWinters(actualSeries,beta=FALSE,gamma=FALSE)%>%
            forecast(h= input$nweeks)
          
          selection2 <- fit %>% accuracy()
        
          selection2
          
        })
          
        
        forecastfunc <- function(){

            Mforecasts <- forecast(amv, h = input$nweeks)
            return(Mforecasts)

        }
        
        
}
    

shinyApp(ui, server)

install.packages("devtools")
devtools::install_github("r-lib/pillar")
getwd()