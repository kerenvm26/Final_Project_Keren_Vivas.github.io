
library(ggplot2)
library(shiny)
library(dplyr)
library(caret)
library(randomForest)

data <- read.csv("C:/Users/kvivasm/OneDrive - North Carolina State University/Documents/ST 558 (601) Data Science for Statisticians/Final_Project_Keren_Vivas.github.io/AQI By State 1980-2022.csv")
data <- data %>%
  rename(
    Good_days = Good.Days,
    Moderate_days = Moderate.Days,
    Unhealthy_days_for_sensitive_groups = Unhealthy.for.Sensitive.Groups.Days, 
    Unhealthy_days = Unhealthy.Days,
    Very_unhealthy_days = Very.Unhealthy.Days,
    Hazardous_days = Hazardous.Days,
    Days_with_high_CO = Days.CO, 
    Days_with_high_NO2 = Days.NO2,
    Days_with_high_Ozone = Days.Ozone,
    Days_with_high_PM2.5 = Days.PM2.5,
    Days_with_high_PM10 = Days.PM10,
    Number_of_counties_measured = Cnty_Rpt,
    Population_estimate = Pop_Est)


ui <- shinyUI(
  navbarPage(
    title = HTML("<b>Air Quality Index Across US States</b>"),
    tabPanel(HTML("<b>About</b>"),
             fluidPage(
               titlePanel(HTML("<b>About This App</b>")),
               wellPanel(
                 h4("Purpose of the App:"),
                 p("This Shiny app is designed to provide users with tools for data exploration and modeling based on the US air quality index dataset."),
                 h4("Data Source:"),
                 p("The data used in this app is sourced from Kaggle's 'Air Quality Index by State (1980-2022)' dataset. You can find more information and access the data", a("here", href = "https://www.kaggle.com/datasets/adampq/air-quality-index-by-state-1980-2022", target = "_blank")),
               ),
               wellPanel(
                 h4("Purpose of Each Tab:"),
                 p("1. 'About': Learn about the app, its purpose, and the data source."),
                 p("2. 'Data Exploration': Explore numerical and graphical summaries of the data."),
                 p("3. 'Modeling': Fit supervised learning models and analyze their performance."),
               ),
               wellPanel(
                 h4("Picture Related to the Data:"),
                 img(src = "AQIbasics.png"),  
               )
             )
    ),
    
    tabPanel(HTML("<b>Data Exploration</b>"),
             fluidPage(
               titlePanel(HTML("<b>Data Exploration</b>")),
               sidebarLayout(
                 sidebarPanel(
                   # Input elements for user customization
                   selectInput("plotType", "Select Plot Type:",
                               choices = c("Histogram", "Boxplot", "Scatter Plot", "Bar Plot")),
                   selectInput("summaryType", "Select Summary Type:",
                               choices = c("Summary", "Mean", "Median", "Standard Deviation")),
                   selectInput("variable1", "Select Variable 1:",
                               choices = c("Good_days", "Moderate_days", "Unhealthy_days_for_sensitive_groups", "Unhealthy_days","Very_unhealthy_days", "Hazardous_days", " Days_with_high_CO", "Days_with_high_NO2", "Days_with_high_Ozone","Days_with_high_PM2.5", "Days_with_high_PM10")),
                   selectInput("variable2", "Select Variable 2:",
                               choices = c("Moderate_days", "Good_days", "Unhealthy_days_for_sensitive_groups", "Unhealthy_days","Very_unhealthy_days", "Hazardous_days", " Days_with_high_CO", "Days_with_high_NO2", "Days_with_high_Ozone","Days_with_high_PM2.5", "Days_with_high_PM10")),
                   selectInput("variable3", "Select Variable 3:",
                               choices = c("Number_of_counties_measured", "Population_estimate")),
                   selectInput("filterVariable", "Filter Variable:",
                               choices = c("Year", "State")),
                   selectInput("filterValue", "Filter Value:",
                               choices = character(0)),
                   actionButton("updateBtn", "Show/Update Summaries and Plots")
                 ),
                 
                 
                 mainPanel(
                   # Additional textOutput elements for displaying summary statistics
                   textOutput("summary_text1"),
                   textOutput("summary_text2"),
                   textOutput("summary_text3"),
                   
                   # Output elements for displaying plots
                   plotOutput("explorationPlot1"),
                   plotOutput("explorationPlot2")
                   
                 )
               )
             )
    ),
    
    tabPanel(HTML("<b>Modeling</b>"),
             tabsetPanel(
               tabPanel(HTML("<b>Modeling Info</b>"),
                        fluidPage(
                          # Add content for Modeling Info tab
                          tags$h3("Modeling Approaches"),
                          tags$p("You can choose between two types of supervised learning models:"),
                          tags$ul(
                            tags$li("Multiple Linear Regression"),
                            tags$li("Random Forest Model")
                          ),
                          tags$h3("Benefits and Drawbacks"),
                          tags$p("Explain the benefits and drawbacks of each model."),
                          tags$h3("Mathematical Explanation"),
                          tags$p("Use mathJax to include mathematical explanations.")
                        )
               ),
               tabPanel(HTML("<b>Model Fitting</b>"),
                        fluidPage(
                          titlePanel(HTML("<b>Model Fitting</b>")),
                          sidebarLayout(
                            sidebarPanel(
                          # Add content for Model Fitting tab
                          numericInput("test_train_split", "Test/Train Split Percentage:", value = 0.8, min = 0, max = 1, step = 0.1),
                          selectInput("response", "Select Response", choices = c("Population_estimate", "Year", "State", "Good_days", "Moderate_days", "Unhealthy_days_for_sensitive_groups", "Unhealthy_days","Very_unhealthy_days", "Hazardous_days", " Days_with_high_CO", "Days_with_high_NO2", "Days_with_high_Ozone","Days_with_high_PM2.5", "Days_with_high_PM10")),
                          selectizeInput("predictors_linear_model", "Select Predictors for Multiple linear model:", choices = c("Population_estimate", "Year", "State", "Good_days", "Moderate_days", "Unhealthy_days_for_sensitive_groups", "Unhealthy_days","Very_unhealthy_days", "Hazardous_days", " Days_with_high_CO", "Days_with_high_NO2", "Days_with_high_Ozone","Days_with_high_PM2.5", "Days_with_high_PM10"), multiple = TRUE),
                          selectizeInput("predictors_randomforest_model", "Select Predictors for Random Forest Model:", choices = c("Year","Population_estimate", "State", "Good_days", "Moderate_days", "Unhealthy_days_for_sensitive_groups", "Unhealthy_days","Very_unhealthy_days", "Hazardous_days", " Days_with_high_CO", "Days_with_high_NO2", "Days_with_high_Ozone","Days_with_high_PM2.5", "Days_with_high_PM10"), multiple = TRUE),
                          textInput("tune_grid", "Tuning Parameter Grid for Random Forest Model:"),
                          numericInput("cv_setting", "Cross-Validation Setting for Random Forest Model:", value = 10, min = 2),
                          actionButton("fit_models_btn", "Fit Models")
                        ),
                        
                        mainPanel(
                          # Add output elements for displaying model fits
                          verbatimTextOutput("model_summaries1"),
                          verbatimTextOutput("model_summaries2")
                        )
               )
               )
               ),
               
               tabPanel(HTML("<b>Prediction</b>"),
                        fluidPage(
                          titlePanel(HTML("<b>Prediction</b>")),
                          sidebarLayout(
                            sidebarPanel(
                          # Add content for Prediction tab
                          selectInput("predictor_value1", "Select Value for Predictor 1:", choices = c("Population_estimate", "Year", "State", "Good_days", "Moderate_days", "Unhealthy_days_for_sensitive_groups", "Unhealthy_days","Very_unhealthy_days", "Hazardous_days", " Days_with_high_CO", "Days_with_high_NO2", "Days_with_high_Ozone","Days_with_high_PM2.5", "Days_with_high_PM10")),
                          selectInput("predictor_value2", "Select Value for Predictor 2:", choices = c("Population_estimate", "Year", "State", "Good_days", "Moderate_days", "Unhealthy_days_for_sensitive_groups", "Unhealthy_days","Very_unhealthy_days", "Hazardous_days", " Days_with_high_CO", "Days_with_high_NO2", "Days_with_high_Ozone","Days_with_high_PM2.5", "Days_with_high_PM10")),
                          actionButton("predict_btn", "Get Predictions"),
               ),
               
                 mainPanel(
                 # Add output elements for displaying predictions
                 verbatimTextOutput("predictions")
             )
    )
    
  )
)

)
)

))