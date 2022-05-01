## FINAL UI

library(shiny)
library(RColorBrewer)
library(rattle)
library(partykit)
library(rpart.plot)
library(DT)
library(ggplot2)
library(shinyjs)

library(shinymaterial)

ui <- material_page(
  # Title
  useShinyjs(),
  titlePanel(title=div(strong("Churn"),
                       img(src="dtree_img3.jpg", height=45, width=55),
                       strong("Prediction"), align="center",
                       style = "font-family: 'Helvetica', sans-serif; font-size: 80%; 
                                text-shadow: 2px 2px 2px #aaa; line-height: 0.1; color: #5c6bc0;"),
             windowTitle = "DT: Churn Prediction"),
  background_color = "grey lighten-5",
  include_nav_bar = FALSE,
  
  # Define tabs
  material_tabs(
    tabs = c(
      "Churn Prediction" = "churn_prediction",
      "Data" = "data",
      "About" = "about"
    ),
  ),
  
  # Churn Prediction Tab
  material_tab_content(
    tab_id = "churn_prediction",
    material_row(
      material_column(
        width = 2,
        material_card(
                      title = h6(strong("Upload Data")),
                      fileInput('churn_upload', 'Churn Data for Training', buttonLabel = "Churn data..."),
                      fileInput('new_upload', 'New Data for Testing', buttonLabel = "New data..."),
                      hr(),
                      h6(strong("Model")),
                      material_dropdown(input_id = "model_dropdown",
                                        label = "Choose the Model",
                                        choices = c("All Data" = "all",
                                                    "Version 1" = "v1",
                                                    "Version 2" = "v2"
                                        ),
                                        selected = "all"
                      ),
                      h6(strong("Show...")),
                      material_switch(input_id = "tree_sw",
                                      off_label = "Off",
                                      on_label = "Visualised Tree",
                                      initial_value = TRUE,
                                      color = "#00bfa5"),
                      material_switch(input_id = "pred_sw",
                                      off_label = "Off",
                                      on_label = "Prediction",
                                      initial_value = TRUE,
                                      color = "#00bfa5"),
                      material_switch(input_id = "confm_sw",
                                      off_label = "Off",
                                      on_label = "Confusion Matrix",
                                      initial_value = FALSE,
                                      color = "#00bfa5"),
                      material_button(
                        input_id = "generate_btn",
                        label = "Generate",
                        icon = "done_all",
                        color = "teal lighten-2"
                      ),
                      br(),
                      br(),
                      downloadButton("downloadData", "Download"),
                      br(),
                      br(),
                      hr(),
                      br(),
                      material_modal(
                        modal_id = "guide_modal",
                        button_text = "Quick Guide",
                        button_icon = "library_books",
                        button_color = "deep-purple darken-1",
                        title = "Churn Prediction Quick Guide",
                        tags$p(strong('STEP 1:'), ' Click ', span('"CHURN DATA..."',  style = "color:teal"), 'to upload Churn Data for training. Make sure training and testing datasets have the same feature names'),
                        tags$p(strong('STEP 2:'), ' Click ', span('"NEW DATA..."',  style = "color:teal"), 'to upload New Data for testing. If you intent to check the Confusion Matrix, New Data must have Churn labels.'),
                        tags$p(strong('STEP 3:'), ' Choose model from the dropdown menu. '), 
                        tags$p(em('Note: All Data model uses all data in the Churn dataset; Version 1 uses first 50% of the dataset; Version 2 uses the last 50% of the dataset.')),
                        tags$p(strong('STEP 4:'), ' Toggle switches to select the tables and/or plot to be generated. '), 
                        tags$p(strong('STEP 5:'), ' Click ', span('"GENERATE"',  style = "color:teal"), 'button to view model output.'),
                        tags$p(strong('STEP 6:'), ' Click ', span('"DOWNLOAD"',  style = "color:teal"), 'button if you wish to download the generated predictions in a .csv file.'),
                      ),
                      divider = TRUE,
                      depth = 2
              ),

          ),
      material_column(
        width = 10, align="center",
        material_card(
          title = strong("Click", span('"Generate"', style = "color:teal"), "to show output."),
          textOutput('model_select'),
          depth = 4,
          textOutput('tree_text'),
          plotOutput("tree", inline = T),
          textOutput('pred_text'),
          DT::dataTableOutput("predictions"),
          textOutput('confm_text'),
          tableOutput("confmatrix"),
        )),
  ),

  ),
  
  # Data Tab
  material_tab_content(
    tab_id = "data",
    material_row(
      material_column(
        width = 2,
        material_card(
          title = h6(strong("View Data")),
          material_switch(input_id = "churn_table",
                          off_label = "Off",
                          on_label = "Show Churn Data",
                          initial_value = TRUE,
                          color = "#00bfa5"),
          material_switch(input_id = "new_table",
                          off_label = "Off",
                          on_label = "Show New Data",
                          initial_value = TRUE,
                          color = "#00bfa5"),
          divider = TRUE,
          depth = 2
        ), 
      ),
      material_column(
        width = 10,
        material_card(
          DT::dataTableOutput("mock_table"),
          DT::dataTableOutput("churndata_table"),
          DT::dataTableOutput("newdata_table"),
          depth = 4
        )
      )
    ),
  ),
  
  # About Tab
  material_tab_content(
    tab_id = "about",
    material_card(
    material_row(
       material_column(width = 8,# offset = 1,
             h3(strong("Decision Trees in Machine Learning")),
             p("A tree has many analogies in real life, and turns out that it has influenced a wide area of machine learning, covering both classification and regression. In decision analysis, a decision tree can be used to visually and explicitly represent decisions and decision making. As the name goes, it uses a tree-like model of decisions. Though a commonly used tool in data mining for deriving a strategy to reach a particular goal, its also widely used in machine learning, which will be the main focus of this article."),
             h4(strong("How can an algorithm be represented as a tree?")),
             p("For this let's consider a very basic example that uses titanic data set for predicting whether a passenger will survive or not. Below model uses 3 features/attributes/columns from the data set, namely sex, age and sibsp (number of spouses or children along)."),
             p("A decision tree is drawn upside down with its root at the top. In the image on the left, the bold text in black represents a condition/internal node, based on which the tree splits into branches/ edges. The end of the branch that doesn't split anymore is the decision/leaf, in this case, whether the passenger died or survived, represented as red and green text respectively.")
       ),
       material_column(width = 3, 
              img(src = "dt_img.png", height = 400, width = 450)
              ),
       material_column(width = 6, offset=2,
              material_card(img(src = "dt_algo.png", height = 343, width = 688, align='center'),
                            depth = 3)
       ),

    ),
    material_row(
        material_column(width = 12,
          material_card(
                        hr(),
                        HTML("<p>App by: <a href='https://github.com/agaknows'>agaknows</a> & <a href='https://github.com/ellouisefm'>ellouisefm</a></p>"),
                        HTML("<p>App code: <a href='https://github.com/ellouisefm/Churn_Prediction_DDP'>Churn Prediction DDP Repository</a></p>"),
                        color = 'indigo lighten-5'
      ))
    )
    )
  
)
)
