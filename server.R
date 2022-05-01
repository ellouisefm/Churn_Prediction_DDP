## FINAL SERVER

library(shiny)
library(RColorBrewer)
library(rattle)
library(partykit)
library(rpart.plot)
library(DT)
library(ggplot2)
library(shinyjs)

library(shinymaterial)

shinyServer(function(input, output, session) {
  # Upload Churn Data
  churn_data <- reactive({
    inFile <- input$churn_upload
    if (is.null(inFile))
      return(NULL)
    churn_data <- read.csv(inFile$datapath, header = TRUE,sep = ",", stringsAsFactors = TRUE)
    return(churn_data)
  })
  
  # Upload New data
  new_data <- reactive({
    inFile <- input$new_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = ",", stringsAsFactors = TRUE)
    return(df)
  })
  
  # Split Churn Data, get top 50% for Version 1 model
  top50 <- reactive({
    df_f50 = churn_data()[1:(0.5*nrow(churn_data())),]
    return(df_f50)
  })
  # Split Churn Data, get last 50% for Version 2 model
  last50 <- reactive({
    df_l50 = churn_data()[(0.5*nrow(churn_data())):nrow(churn_data()),]
    return(df_l50)
  })
  
  # Data Tab, output churn data table
  output$churndata_table <- DT::renderDataTable({
    if(input$churn_table){
      churn_data()
    }
  }, options = list(scrollX = TRUE), caption = "Churn Data")
  # Data Tab, output new data table
  output$newdata_table<- DT::renderDataTable({
    if(input$new_table){
      new_data()
    }
  }, options = list(scrollX = TRUE), caption = "New Data")
  # Variables change per selection
  training_data <- reactiveValues(data=NULL)
  text_temp <- reactiveValues(data=NULL)
  # Change training data based on model selected
  observe(
    if(input$model_dropdown == "all"){
      training_data$data <- churn_data()
      text_temp$data <- "ALL DATA"
    }
    else if(input$model_dropdown == "v1"){
      training_data$data <- top50()
      text_temp$data <- "VERSION 1"
    }
    else if(input$model_dropdown == "v2"){
      training_data$data <- last50()
      text_temp$data <- "VERSION 2"
  }
  )
  # Model selected text output when you generate
  model_text <- eventReactive(input$generate_btn,{
    text_out <- text_temp$data
    return(text_out)
  }, ignoreInit = TRUE)
  output$model_select <- renderText({
    model_text()
  })
  
  # Model
  tree_model <- eventReactive(input$generate_btn,{
    all_model = rpart(Churn.~
                        Account.Length +
                        Int.l.Plan +
                        VMail.Plan +
                        VMail.Message +
                        Day.Mins +
                        Day.Calls +
                        Day.Charge +
                        Eve.Mins +
                        Eve.Calls +
                        Eve.Charge +
                        Night.Mins +
                        Night.Calls +
                        Night.Charge +
                        Intl.Mins +
                        Intl.Calls +
                        Intl.Charge +
                        CustServ.Calls,
                      data = training_data$data)
    return(all_model)
  }, ignoreInit = TRUE)
  
  # Temp Values for Tree plot, prediction and confusion matrix
  tree_text_temp <- reactiveValues(data=NULL)
  pred_text_temp <- reactiveValues(data=NULL)
  confm_text_temp <- reactiveValues(data=NULL)
  pred_temp <- reactiveValues(data=NULL)
  confm_temp <- reactiveValues(data=NULL)
  # switch condition to view output
  observe(
    if(input$tree_sw){
      tree_text_temp$data <- "===================================================== TREE PLOT ====================================================="
    }
    else if (!input$tree_sw) {
      tree_text_temp$data <- NULL
    }
  )
  observe(
    if(input$pred_sw){
      pred_text_temp$data <- "==================================================== PREDICTION ===================================================="
      df_final = new_data()
      df_final$Prediction = predict(tree_model(), new_data(), type='class')
      pred_temp$data <- df_final
    }
    else if (!input$pred_sw) {
      pred_text_temp$data <- NULL
      pred_temp$data <- NULL
    }
  )
  observe(
    if(input$confm_sw){
      confm_text_temp$data <- "================================================== CONFUSION MATRIX =================================================="
      confm_temp$data <- table(predict(tree_model(), new_data(), type='class'), new_data()$Churn.)
    }
    else if (!input$confm_sw) {
      confm_text_temp$data <- NULL
      confm_temp$data <- NULL
    }
  )
  # generate output when button clicked
  tree_text_out <- eventReactive(input$generate_btn, {
    text <- tree_text_temp$data
    return(text)
  }, ignoreInit = TRUE)
  
  pred_text_out <- eventReactive(input$generate_btn,{
    text <- pred_text_temp$data
    return(text)
  }, ignoreInit = TRUE)
  
  confm_text_out <- eventReactive(input$generate_btn,{
    text <- confm_text_temp$data
    return(text)
  }, ignoreInit = TRUE)
  
  output$tree_text <- renderText({
      tree_text_out()
  })
  
  output$pred_text <- renderText({
      pred_text_out()
  })
  
  output$confm_text <- renderText({ 
      confm_text_out()
  })

  output$tree = renderPlot({
    fancyRpartPlot(tree_model(), palettes="BuGn")
  }, height = 1200/2.25, width = 1200)

  observeEvent(input$generate_btn,{
    if(input$tree_sw){
      show("tree")
    }
    else if(!input$tree_sw){
     hide("tree")
    }
  }, ignoreInit = TRUE)

  predict_out <- eventReactive(input$generate_btn,{
    pred_final <- pred_temp$data
    return(pred_final)
  }, ignoreInit = TRUE)

  output$predictions <- DT::renderDataTable({
      predict_out()
  }, options = list(scrollX = TRUE))

  confmat <- eventReactive(input$generate_btn,{
    confmat_final <- confm_temp$data
    return(confmat_final)
  })

  output$confmatrix <- renderTable({
      confmat()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("churn_prediction_",input$model_dropdown, ".csv", sep = "")
      }, 
    content = function(fname){
      write.csv(predict_out(), fname)
    }
  )
  
})
