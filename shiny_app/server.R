#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
library(shiny)
library(dplyr)
library(googleVis)
library(plotly)
library(ggplot2)
library(lmtest)
library(reshape)
library(markdown)
library(rmarkdown)
library(yaml)
library(knitr)


country_codes <- read.csv("https://datahub.io/core/currency-codes/r/codes-all.csv",sep=",",dec=".",header=T,stringsAsFactor=F)
country_codes <- country_codes[c("Entity", "AlphabeticCode")]
# manual correction
country_codes[which(country_codes$AlphabeticCode=="GBP"),]$Entity = "UNITED KINGDOM"
country_codes[which(country_codes$AlphabeticCode=="HKD"),]$Entity = "CHINA"
country_codes[which(country_codes$AlphabeticCode=="RUB"),]$Entity = "RUSSIA"
country_codes[which(country_codes$AlphabeticCode=="CZK"),]$Entity = "CZECH REPUBLIC"
country_codes[which(country_codes$AlphabeticCode=="PHP"),]$Entity = "PHILIPPINES"
country_codes[which(country_codes$AlphabeticCode=="KRW"),]$Entity = "SOUTH KOREA"
country_codes[which(country_codes$Entity=="UNITED STATES OF AMERICA (THE)"),]$Entity = "US"


shinyServer(function(input, output) {

    
    # ===================================== REACTIVE VALUES AND SUCH  =====================================
    v <- reactiveValues(dataLoadDownload = FALSE)
    
    w <- reactiveValues(dataRegressionBuilder = FALSE)
    
    outVar1 <- reactiveValues(
        selectYearVar = "2013"
    )
    
    outVar2 <- reactiveValues(
        selectYearVar = format(Sys.Date(), "%Y")
    )
    
    observeEvent(input$selectYear1,{
        outVar1$selectYearVar <- input$selectYear1
    })
    
    observeEvent(input$selectYear2,{
        outVar2$selectYearVar <- input$selectYear2
    })

    observeEvent(input$getDataFromServer,{
        v$dataLoadDownload <- !v$dataLoadDownload
    })
    
    observeEvent(input$buildRegression,{
        w$dataRegressionBuilder <- !w$dataRegressionBuilder
    })
    
    
    # ===================================== DATA BLOCK ===================================== 
    
    dataIn <- reactive({
        
        ret <- data.frame()
        big_ret <- data.frame()
    
        if(v$dataLoadDownload==TRUE){

            
            if(outVar1$selectYearVar <= outVar2$selectYearVar && outVar1$selectYearVar>=2013){
                
                # Clean up every table in a loop and join into big dataframe that is being returned
                for(year in seq(outVar1$selectYearVar, outVar2$selectYearVar)) {
                    
                    fileName <- paste0(year,"_NBP_data.csv")
                    
                    try({
                        if(file.exists(fileName) && as.Date(file.info(fileName)$mtime)==Sys.Date()){
                            # Local loading block
                            #cat(paste("Reading data from local file\n"))
                            ret <- read.table(file=fileName,sep=";",dec=",",header=T,stringsAsFactor=F)
                            big_ret <- bind_rows(big_ret, ret)
                        } else {
                            # Scraping block
                            res <- try({
                                
                                d <- readLines(paste0("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_",year,".csv"))
                                d <- d[-2]
                                d <- d[-c((length(d)-3):length(d))]
                                
                                tmpColnames <- strsplit(d[1],";",useBytes=T)[[1]]
                                tmpColnames <- tmpColnames[-c((length(tmpColnames)-1):length(tmpColnames))]
                                d <- do.call("rbind",
                                             lapply(strsplit(d[-1],";"),
                                                    function(x){
                                                        matrix(as.numeric(gsub(",",".",x[-c((length(x)-1):length(x))])),nrow=1)
                                                    })
                                )
                                colnames(d) <- tmpColnames
                                d <- as.data.frame(d)
                                
                                d$data <- as.Date(as.character(d$data),format="%Y%m%d")
                                ret <- d
                                
                                big_ret <- bind_rows(big_ret, ret)
                                
                                write.table(ret,file=fileName,sep=";",dec=",",row.names=F)
                                
                            }, silent=T)
                            
                            if(inherits(res,"try-error")){
                                cat(paste("An error occurred while downloading data!!!\n")) 
                            }
                        }
                    })
                }
                
                
            } else {
                cat(paste("Year range is incorrect or not supported."))
            }
        }
        
        
        big_ret <- big_ret[order(as.Date(big_ret$data, format="%Y-%m-%d"), decreasing = T),]
        
        return(big_ret)
    })
    
    comparisonFrame <- reactive({
        
        data_cols = colnames(dataIn())
        
        # just the currency names (no X... in front)
        substrRight <- function(x, n=3){
            substr(x, nchar(x)-n+1, nchar(x))
        }
        
        data_cols <- lapply(data_cols,substrRight)
        
        # check if they are different and make a new smaller table if so
        if(input$currency1 != input$currency2) {
        
            comparison_frame = cbind(dataIn()[which(data_cols=="ata")],    #dataIn()$data,
                                 dataIn()[which(data_cols==input$currency1)],
                                 dataIn()[which(data_cols==input$currency2)])
        
        } else {
            
            comparison_frame = cbind(dataIn()[which(data_cols=="ata")],    #dataIn()$data,
                                     dataIn()[which(data_cols==input$currency1)])
        }
    
        return(comparison_frame)
        
    })
    
    mapHelperFrame <- reactive ({
        
        # frame with countries where selected currencies are valid
        little_frame <-  rbind(country_codes[which(country_codes$AlphabeticCode==input$currency1),],
                         country_codes[which(country_codes$AlphabeticCode==input$currency2),])
        # currency number
        little_frame$CurrNo <- ifelse(little_frame$AlphabeticCode == input$currency1, 1, 2)
        # info when highlighted
        little_frame$Info <- paste(little_frame$Entity, little_frame$AlphabeticCode, sep=": ")
        
        return(little_frame)
        
        
    })
    
    seriesHelperFrame <- reactive ({
        
        # for simplicity
        ret <- comparisonFrame()[order(as.Date(comparisonFrame()$data, format="%Y-%m-%d"), decreasing = F),]
        return(ret)
        
        
    })
    
    # Main Table rendering code
    output$dataSample <- DT::renderDataTable({
        DT::datatable(  
            dataIn(), 
            rownames = FALSE,
            options = list(
                scrollX = TRUE,
                pageLength = 10,
                lengthMenu = seq(from=10,by=10,to=100) 
            )
        )
    })
    
    # Comparison table rendering code
    output$compSample <- DT::renderDataTable({

        DT::datatable(  
            comparisonFrame(), 
            rownames = FALSE,
            options = list(
                scrollX = TRUE,
                pageLength = 10,
                lengthMenu = seq(from=10,by=10,to=100) 
            )
        )

    })
    
    # ======================================= VIS BLOCK ===================================
    
    # MAP
    output$visSample <- renderGvis({
        
        gvisGeoChart(mapHelperFrame(), locationvar="Entity", colorvar="CurrNo", 
                     hovervar="Info",
                     options=list(projection="kavrayskiy-vii",width=1200,height=800,
                                  colors="['#000000', '#FFD700']",
                                  title="Countries with selected currencies"))
        
    })
    
    # SERIES
    output$seriesSample <- renderGvis({
        
        Line <- gvisLineChart(seriesHelperFrame(), xvar="data",
                              options=list(width=1200, height=800,
                                           colors="['#000000', '#FFD700']",
                                           title="Time series of selected currencies", 
                                           vAxis="{title:'Rate'}",
                                           hAxis="{title:'Date'}"))
        
        
    })
    
    # BARPLOT
    output$barplot <- renderPlotly({
        
        if(nrow(comparisonFrame()) && input$currency1 != input$currency2){
            
            df <- comparisonFrame()
            df$difference = unlist(abs(df[2] - df[3]))
            
            img <- (
                ggplot(df, mapping = aes(x = difference)) +
                geom_histogram(color="#000000", fill="#FFD700") +
                ggtitle("Barplot of currency differences") + 
                xlab("Difference") + ylab("Frequency")
            )
            
            
            # as plotly 
            img <- ggplotly(img)
        }else{
            img <- ggplot(data.frame())
        }
        
        return(img)
        
    })
    
    # SCATTERPLOT
    output$scatterplot <- renderPlotly({
        
        
        
        if(nrow(comparisonFrame()) && input$currency1 != input$currency2){
            
            df <- comparisonFrame()
            
            img <- (
                ggplot(df, mapping = aes_string(x = names(df)[2], y = names(df)[3])) +
                geom_point() +
                scale_fill_manual(values=c('#000000','#FFD700')) + 
                ggtitle("Scatterplot of selected currencies") + 
                xlab(names(df)[2]) + ylab(names(df)[3])
            )
            
            
            # as plotly 
            img <- ggplotly(img)
        }else{
            img <- ggplot(data.frame())
        }
        
        return(img)
    })
    
    # ======================================= REGRESSION BLOCK ===================================
    
    # SUMMARY
    output$summaryInfo <- renderPrint({
        
        if(w$dataRegressionBuilder){
        
            df <- comparisonFrame()
            # for simplicity
            colnames(df) <- c("data", "x", "y") 
            
            regressionModel = lm(formula = x ~ y, data = df)
            ret <- summary(regressionModel)
            
            return(ret)
        }
    })
    
    # RAINTEST
    output$rainTest <- renderPrint({
        
        if(w$dataRegressionBuilder==TRUE){
            
            df <- comparisonFrame()
            colnames(df) <- c("data", "x", "y") 
            
            regressionModel = lm(formula = x ~ y, data = df)
            ret <- raintest(regressionModel)
            
            return(ret)
        
        }
        
    })
    
    # PLOT
    output$regression <- renderPlotly({
        
        # if the button is pressed and the currencies differ only then go throught with it
        if(w$dataRegressionBuilder){
        
            if(nrow(comparisonFrame()) && input$currency1 != input$currency2){
                
                df <- comparisonFrame()
                cF_names <- colnames(df)
                colnames(df) <- c("data", "x", "y") 
                
                img <- (
                    ggplot(df, mapping = aes_string(x = names(df)[2], y = names(df)[3]))
                    + geom_point()
                    + geom_smooth(method='lm',formula= y ~ x, colour="#FFD700")
                    + ggtitle("Regression model - graphical representation") 
                    + xlab(names(df)[2]) + ylab(names(df)[3])
                )
                
                
                # as plotly
                img <- ggplotly(img)
            }else{
                img <- ggplot(data.frame())
            }
            
            return(img)
            
        }
        
    })
    
    # ======================================= REPORTING BLOCK ===================================
    
    output$rmdReport <- downloadHandler(
        
        # make the file name to be the current date
        filename = function() { 
            
            return(paste0(getwd(), "/", gsub("-","_",as.character(Sys.Date())),"_out.html")) 
        },
        
        content = function(file) {
            
            # create everything that needs to be passed to the rmd
            regression_flag = w$dataRegressionBuilder
            misc_df = data.frame("curr1" = input$currency1, "curr2" = input$currency2,
                                 "rflag" = regression_flag)
            cF = comparisonFrame()
            mHF = mapHelperFrame()
            sHF = seriesHelperFrame()
            
            # EVERYTHING NEEDED FOR INTERACTIVE REPORTING HAS TO BE SAVED FIRST 
            # yamls work if interactivity can be sacrificed
            write.table(misc_df,file="miscFrame.csv",sep=";",dec=",",row.names=F)
            #yaml_data <- as.yaml(dataIn())
            #yaml_cf <- as.yaml(comparisonFrame())
            write.table(cF,file="comparisonFrame.csv",sep=";",dec=",",row.names=F)
            #yaml_mhf <- as.yaml(mapHelperFrame())
            write.table(mHF,file="mapHelperFrame.csv",sep=";",dec=",",row.names=F)
            #yaml_shf <- as.yaml(seriesHelperFrame())
            write.table(sHF,file="seriesHelperFrame.csv",sep=";",dec=",",row.names=F)
            
            tempReport <- file.path(tempdir(), "raport.Rmd")
            file.copy("raport.Rmd", tempReport, overwrite = TRUE)
            
            knit(input="raport.Rmd", output="tmp.md")
            # Convert to HTML
            markdownToHTML(file="tmp.md", output="Raport.html")
            unlink("tmp.md")
        }
        
    )

})

