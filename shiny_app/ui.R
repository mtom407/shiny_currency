#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)
library(plotly)


currentYear = format(Sys.Date(), "%Y")

all_curr_choices = as.character(c("THB", "USD", "AUD", "HKD", "CAD", "NZD", "SGD", "EUR", "HUF", 
                             "CHF", "GBP", "UAH", "JPY", "CZK", "DKK", "ISK", "NOK", "SEK",
                             "HRK", "RON", "BGN", "TRY", "LTL", "LVL", "ILS", "CLP", "PHP",
                             "MXN", "ZAR", "BRL", "MYR", "RUB", "IDR", "INR", "KRW", "CNY", "XDR"))


shinyUI(fluidPage(

    
    titlePanel("Daily exchange rates. Source: NBP"),

    
    sidebarLayout(
        
        sidebarPanel(
            
            actionButton("getDataFromServer", label = "Download data"),
            
            selectInput("selectYear1",
                        label = "From year",
                        choices = as.vector(as.character(2013:currentYear),mode="list")
            ),
            selectInput("selectYear2",
                        label = "To year",
                        choices = as.vector(as.character(currentYear:2013),mode="list")
            ),
            selectInput("currency1",
                        label="First currency",
                        choices = as.vector(all_curr_choices, mode="list")
            ),
            selectInput("currency2",
                        label="Second currency",
                        choices = as.vector(all_curr_choices, mode="list")
            ),
            actionButton("buildRegression", label = "Build regression model"),
            
            downloadButton("rmdReport",label = "Create HTML report")
        ),

        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Main table", DT::dataTableOutput("dataSample")),
                        
                        tabPanel("Comparison - table", DT::dataTableOutput("compSample")),
                        
                        tabPanel("Comparison - vis", htmlOutput("visSample"),
                                                              htmlOutput("seriesSample"), 
                                                              plotlyOutput("barplot"),
                                                              plotlyOutput("scatterplot")),
                        tabPanel("Regression model", verbatimTextOutput("summaryInfo"),
                                                   verbatimTextOutput("rainTest"),
                                                   plotlyOutput("regression"))
            )
            
        )
    )
))
