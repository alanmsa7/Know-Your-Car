library(shiny)
library(ggplot2)
library(gridExtra)
library(rsconnect)
par(mfrow = c(2,2))
shinyServer(
  function(input, output, session){
    
    ##Load datasets
    library(readxl)
    DEMO <- read_excel("NEWDATA.xlsx")
    
    ## Create the optimum multivariate linear regression model
    
    
    fit <- lm(PRICE ~ DISPLACEMENT + AGE + ALTO800STD + ALTO800LXI + ALTO800VXI + SWIFTLXI + SWIFTVXI + SWIFTZXI + SWIFTLDI + SWIFTVDI + SWIFTZDI + DZIRELXI + DZIREVXI + DZIREZXI + DZIRELDI + DZIREVDI + DZIREZDI + WAGONRLXI + WAGONRVXI + CELERIOLXI + CELERIOVXI + CELERIOZXI + RITZLXI + RITZVXI + RITZZXI + RITZLDI + RITZVDI + RITZZDI + OWNERS + TRANSMISSION , data=DEMO)

    
    
    #Reactive ui
    
    
    observeEvent(input$MODEL,{
      
      if(input$MODEL == 'Swift'){
        updateRadioButtons(session=session,inputId =  "VERSION",choices = c('LDI','VDI','LXI','VXI','ZXI','ZDI'))
        updateRadioButtons(session=session,inputId =  "TRANSMISSION",choices = c('Manual'))
      }
      else if (input$MODEL == 'Alto800'){
        updateRadioButtons(session=session, inputId =  "VERSION",choices = c('LXI','VXI','STD'))
        updateRadioButtons(session=session,inputId =  "TRANSMISSION",choices = c('Manual'))
      }
      else if (input$MODEL == 'Wagonr'){
        updateRadioButtons(session=session,inputId =  "VERSION",choices = c('LXI','VXI'))
        updateRadioButtons(session=session,inputId =  "TRANSMISSION",choices = c('Manual'))
      }
      else if (input$MODEL == 'Ritz'){
        updateRadioButtons(session=session,inputId =  "VERSION",choices = c('LXI','VXI','ZXI','LDI','VDI','ZDI'))
        updateRadioButtons(session=session,inputId =  "TRANSMISSION",choices = c('Manual'))
      }
      else if (input$MODEL == 'Celerio'){
        updateRadioButtons(session=session,inputId =  "VERSION",choices = c('LXI','VXI','ZXI'))
        observeEvent(input$VERSION,{
          if(input$VERSION!='LXI'){
          updateRadioButtons(session=session,inputId =  "TRANSMISSION",choices = c('Manual','Auto'))
        }
        else{
          updateRadioButtons(session=session,inputId =  "TRANSMISSION",choices = c('Manual'))
        }
      })
      }
      else if (input$MODEL == 'Dzire'){
        updateRadioButtons(session=session,inputId =  "VERSION",choices = c('LXI','VXI','ZXI','LDI','VDI','ZDI'))
        updateRadioButtons(session=session,inputId =  "TRANSMISSION",choices = c('Manual'))
      }
    })
    
    
        
    
    #Prediction
    
    
  observeEvent(input$PREDICTPRICE, {
  output$prediction <- renderText  ({ 
  
  isolate({
    
    #INPUTS FROM UI
    
    MOD <- input$MODEL
    VERS <- input$VERSION
    OWNED<- as.integer(input$OWNERSHIP)
    DISP <- as.integer(input$DISPLACEMENT)
    AG <- as.integer(input$AGE)
    TRAN<-input$TRANSMISSION
    
    
    
    
    trans<-'MANUAL'
    alto800LXI<-0
    alto800VXI<-0
    alto800STD<-0
    swiftLXI<-0
    swiftVXI<-0
    swiftLDI<-0
    swiftVDI<-0
    swiftZXI<-0
    swiftZDI<-0
    wagonrLXI<-0
    wagonrVXI<-0
    ritzLXI<-0
    ritzVXI<-0
    ritzLDI<-0
    ritzVDI<-0
    ritzZXI<-0
    ritzZDI<-0
    celerioLXI<-0
    celerioVXI<-0
    celerioZXI<-0    
    dzireLXI<-0
    dzireVXI<-0
    dzireLDI<-0
    dzireVDI<-0
    dzireZXI<-0
    dzireZDI<-0
    owners<-0
    
    
    
    ##VERSION AND MODEL ISOLATION
            
    if(MOD == 'Swift')
      {
      if(VERS == 'LDI'){
        swiftLDI <- 1
      }
      else if (VERS == 'VDI'){
        swiftVDI <- 1
      }
      else if (VERS == 'LXI'){
        swiftLXI <- 1
      }
      else if (VERS == 'VXI'){
        swiftVXI <- 1
      }
      else if (VERS == 'VDI'){
        swiftVDI <- 1
      }
      else if (VERS == 'ZXI'){
        swiftZXI <- 1
      }
      else if (VERS == 'ZDI'){
        swiftZDI <- 1
      }
      
    }
    else if (MOD == 'Alto800')
      {
      if (VERS == 'LXI'){
        alto800LXI <- 1
        }
      else if (VERS == 'VXI'){
        alto800VXI <- 1
        }
      else if (VERS == 'STD'){
        alto800STD <- 1
        }
      }


    else if(MOD == 'Dzire')
    {
      if(VERS == 'LDI'){
        dzireLDI <- 1
      }
      else if (VERS == 'VDI'){
        dzireVDI <- 1
      }
      else if (VERS == 'LXI'){
        dzireLXI <- 1
      }
      else if (VERS == 'VXI'){
        dzireVXI <- 1
      }
      else if (VERS == 'VDI'){
        dzireVDI <- 1
      }
      else if (VERS == 'ZXI'){
        dzireZXI<- 1
      }
      else if (VERS == 'ZDI'){
        dzireZDI <- 1
      }
      
    }
    
    else if(MOD == 'Ritz')
    {
      if(VERS == 'LDI'){
        ritzLDI <- 1
      }
      else if (VERS == 'VDI'){
        ritzVDI <- 1
      }
      else if (VERS == 'LXI'){
        ritzLXI <- 1
      }
      else if (VERS == 'VXI'){
        ritzVXI <- 1
      }
      else if (VERS == 'VDI'){
        ritzVDI <- 1
      }
      else if (VERS == 'ZXI'){
        ritzZXI<- 1
      }
      else if (VERS == 'ZDI'){
        ritzZDI <- 1
      }
      
    }

    else if (MOD == 'Wagonr')
    {
      
      if (VERS == 'LXI'){
        wagonrLXI <- 1
      }
      else if (VERS == 'VXI'){
        wagonrLXI <- 1
      }
    }
    
    
    else if (MOD == 'Celerio')
    {
      if (VERS == 'LXI'){
        celerioLXI <- 1
      }
      else if (VERS == 'VXI'){
        celerioVXI <- 1
        if(TRAN=='Auto')
          {
          trans<-'AUTO' 
          }
      }
      else if (VERS == 'ZXI'){
        celerioZXI <- 1
        if(TRAN=='Auto')
        {
          trans<-'AUTO' 
        }
      }
    }
    
    
    
    
    
    ##framing data
    
    newData <- data.frame(AGE = AG,
                     DISPLACEMENT = DISP,
                     ALTO800STD = alto800STD,
                     ALTO800LXI = alto800LXI,
                     ALTO800VXI = alto800VXI,
                     SWIFTLXI = swiftLXI,
                     SWIFTVXI = swiftVXI,
                     SWIFTZXI = swiftZXI,
                     SWIFTLDI = swiftLDI,
                     SWIFTVDI = swiftVDI,
                     SWIFTZDI = swiftZDI,
                     DZIRELXI = dzireLXI,
                     DZIREVXI = dzireVXI,
                     DZIREZXI = dzireZXI,
                     DZIRELDI = dzireLDI,
                     DZIREVDI = dzireVDI,
                     DZIREZDI = dzireZDI,
                     WAGONRLXI = wagonrLXI,
                     WAGONRVXI = wagonrVXI,
                     CELERIOLXI = celerioLXI,
                     CELERIOVXI = celerioVXI,
                     CELERIOZXI = celerioZXI,
                     RITZLXI = ritzLXI,
                     RITZVXI = ritzVXI,
                     RITZZXI = ritzZXI, 
                     RITZLDI = ritzLDI,
                     RITZVDI = ritzVDI,
                     RITZZDI = ritzZDI,
                     OWNERS = OWNED,
                     TRANSMISSION=trans
                     )
    predict(fit, newData)
                  })
              })
  
  ##plotting graph
  
  output$Plot<-renderPlot({
    p1<-ggplot(data = DEMO , aes(x = AGE, y = PRICE))+geom_smooth(method = 'lm')+geom_point()
    p2<-ggplot(data = DEMO , aes(x = DISPLACEMENT, y = PRICE))+geom_smooth(method = 'lm')+geom_point()
    p3<-ggplot(data = DEMO , aes(x = OWNERS , y = PRICE))+geom_smooth(method = 'lm')+geom_point()
    p4<-ggplot(data = DEMO , aes(x = TRANSMISSION , y = PRICE))+geom_point()
    grid.arrange(p1,p2,p3,p4,nrow = 2, ncol = 2)
  })
  
  
          })

  })
    
