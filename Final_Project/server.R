library(shiny)

server <- shinyServer(function(input, output, session) {
  
  # Dynamically update choices for filterValue based on selected filterVariable
  observe({
    variable_choices <- c("Year", "State")
    selected_variable <- input$filterVariable
    
    if (selected_variable %in% variable_choices) {
      choices <- unique(data[[selected_variable]])
      updateSelectInput(session, "filterValue", choices = choices)
    }
  })
  
  # Server logic for the "Data Exploration" tab
  observeEvent(input$updateBtn, {
    
    # Retrieve user inputs
    plotType <- input$plotType
    summaryType <- input$summaryType
    variable1 <- input$variable1
    variable2 <- input$variable2
    variable3 <- input$variable3
    filterVariable <- input$filterVariable
    filterValue <- input$filterValue
    
    # Filter data based on user input
    filtered_data <- data
    if (!is.null(filterVariable) && !is.null(filterValue)) {
      filtered_data <- subset(filtered_data, get(filterVariable) == filterValue)
    }
    
    
    # Calculate and render summary statistics for each variable
    output$summary_text1 <- renderPrint({
      variable_summary <- ifelse(is.numeric(filtered_data[[variable1]]), 
                                 switch(input$summaryType,
                                        "Mean" = round(mean(filtered_data[[variable1]]), 2),
                                        "Median" = round(median(filtered_data[[variable1]]), 2),
                                        "Standard Deviation" = round(sd(filtered_data[[variable1]]), 2),
                                        "Summary" = {
                                          paste("Length:", length(filtered_data[[variable1]]),
                                                "Min:", round(min(filtered_data[[variable1]]), 2),
                                                "Max:", round(max(filtered_data[[variable1]]), 2),
                                                "Mean:", round(mean(filtered_data[[variable1]]), 2),
                                                "Median:", round(median(filtered_data[[variable1]]), 2),
                                                "Standard Deviation:", round(sd(filtered_data[[variable1]]), 2))
                                        }
                                 ), "Not applicable")
      paste(input$summaryType, "of", variable1, ":", variable_summary)
    })
    
    output$summary_text2 <- renderPrint({
      variable_summary <- ifelse(is.numeric(filtered_data[[variable2]]), 
                                 switch(input$summaryType,
                                        "Mean" = round(mean(filtered_data[[variable2]]), 2),
                                        "Median" = round(median(filtered_data[[variable2]]), 2),
                                        "Standard Deviation" = round(sd(filtered_data[[variable2]]), 2),
                                        "Summary" = {
                                          paste("Length:", length(filtered_data[[variable2]]),
                                                "Min:", round(min(filtered_data[[variable2]]), 2),
                                                "Max:", round(max(filtered_data[[variable2]]), 2),
                                                "Mean:", round(mean(filtered_data[[variable2]]), 2),
                                                "Median:", round(median(filtered_data[[variable2]]), 2),
                                                "Standard Deviation:", round(sd(filtered_data[[variable2]]), 2))
                                        }
                                 ), "Not applicable")
      paste(input$summaryType, "of", variable2, ":", variable_summary)
    })
    
    output$summary_text3 <- renderPrint({
      variable_summary <- ifelse(is.numeric(filtered_data[[variable3]]), 
                                 switch(input$summaryType,
                                        "Mean" = round(mean(filtered_data[[variable3]]), 2),
                                        "Median" = round(median(filtered_data[[variable3]]), 2),
                                        "Standard Deviation" = round(sd(filtered_data[[variable3]]), 2),
                                        "Summary" = {
                                          paste("Length:", length(filtered_data[[variable3]]),
                                                "Min:", round(min(filtered_data[[variable3]]), 2),
                                                "Max:", round(max(filtered_data[[variable3]]), 2),
                                                "Mean:", round(mean(filtered_data[[variable3]]), 2),
                                                "Median:", round(median(filtered_data[[variable3]]), 2),
                                                "Standard Deviation:", round(sd(filtered_data[[variable3]]), 2))
                                        }
                                 ), "Not applicable")
      paste(input$summaryType, "of", variable3, ":", variable_summary)
    })
    
    # Create plots based on user input
    if (plotType == "Histogram") {
      output$explorationPlot1 <- renderPlot({
        ggplot(filtered_data, aes(x = filtered_data[[variable1]])) +
          geom_histogram(fill = "lightblue", color = "black", bins = 20) +
          labs(title = "Histogram of Numeric Data",
               x = variable1,
               y = "Frequency")
      })
      
      output$explorationPlot2 <- renderPlot({
        ggplot(filtered_data, aes(x = filtered_data[[variable2]])) +
          geom_histogram(fill = "lightblue", color = "black", bins = 20) +
          labs(title = "Histogram of Numeric Data",
               x = variable2,
               y = "Frequency")
      })
      
    } else if (plotType == "Boxplot") {
      output$explorationPlot1 <- renderPlot({
        ggplot(filtered_data, aes(x = filtered_data[[variable1]])) +
          geom_boxplot(fill = "lightblue", color = "black") +
          labs(title = "Boxplot of Numeric Data",
               x = variable1,
               y = "Values")
      })
      
      output$explorationPlot2 <- renderPlot({
        ggplot(filtered_data, aes(x = filtered_data[[variable2]])) +
          geom_boxplot(fill = "lightblue", color = "black") +
          labs(title = "Boxplot of Numeric Data",
               x = variable2,
               y = "Values")
      })
      
    } else if (plotType == "Scatter Plot") {
      output$explorationPlot1 <- renderPlot({
        ggplot(filtered_data, aes(x = filtered_data[[variable1]], y = filtered_data[[variable2]], size = filtered_data[[variable3]])) +
          geom_point(color = "blue") +
          scale_size() +  
          labs(title = "Scatter Plot of Variable1 vs. Variable2",
               x = variable1,
               y = variable2,
               size = "Population Estimate")
      })
      
      output$explorationPlot2 <- renderPlot({
        ggplot(filtered_data, aes(x = filtered_data[[variable2]], y = filtered_data[[variable1]], size = filtered_data[[variable3]])) +
          geom_point(color = "blue") +
          scale_size() +  
          labs(title = "Scatter Plot of Variable2 vs. Variable1",
               x = variable2,
               y = variable1,
               size = "Population Estimate")
      })
      
    } else if (plotType == "Bar Plot") {
      output$explorationPlot1 <- renderPlot({
        ggplot(filtered_data, aes(x = filtered_data[[variable1]])) +
          geom_bar(position = "dodge", color = "black") +
          labs(title = "Bar Chart of Variable1 and Variable2",
               x = variable1,
               y = "Count") +
          scale_fill_manual(values = c("lightblue", "pink"))  
      })
      
      output$explorationPlot2 <- renderPlot({
        ggplot(filtered_data, aes(x = filtered_data[[variable2]])) +
          geom_bar(position = "dodge", color = "black") +
          labs(title = "Bar Chart of Variable2",
               x = variable2,
               y = "Count") +
          scale_fill_manual(values = c("lightblue", "pink"))  
      })
    }
  })
  
    # Define reactive values to store model objects
    models <- reactiveValues(model1 = NULL, model2 = NULL)
    model_metrics <- reactiveValues(rmse_model1 = NULL, rmse_model2 = NULL)
    
    
    # Fit models on button click
    observeEvent(input$fit_models_btn, {
      
      # Retrieve user inputs
      test_train_split <- input$test_train_split
      response <- input$response
      predictors_linear_model <- input$predictors_linear_model
      predictors_rf_model <- input$predictors_randomforest_model
      tune_grid <- input$tune_grid
      cv_setting <- input$cv_setting
      
      # Split data into training and testing sets
      set.seed(123)  # Set seed for reproducibility
      train_index <- createDataPartition(data$response, p = test_train_split, list = FALSE)
      train_set <- data[train_index, ]
      test_set <- data[-train_index, ]
      
      # Fit and evaluate linear model
      linear_model <- lm(response ~ ., data = data[, c("response", predictors_linear_model)])
      models$model1 <- linear_model
      
      # Get predictions on the training set
      linear_predictions_train <- predict(linear_model, newdata = data)
      
      # Calculate RMSE on the training set
      rmse_linear_train <- sqrt(mean((linear_predictions_train - data$response)^2))
      
      # Display model summaries
      output$model_summaries1 <- renderPrint({
        cat("Linear Model Summary:\n")
        print(summary(linear_model))  # Use summary() for linear models
        cat("RMSE for Linear Model on Training Data:", rmse_linear_train, "\n")
      })
      
      # Fit and evaluate random forest model
      rf_model <- train(
        response ~ .,
        data = data,
        method = "rf",
        trControl = trainControl(method = "cv", number = cv_setting, verboseIter = TRUE),
        tuneGrid = tuning_grid
      )
      
      # Get predictions on the training set
      rf_predictions_train <- predict(rf_model, newdata = data)
      
      # Calculate RMSE on the training set
      rmse_rf_train <- sqrt(mean((rf_predictions_train - data$response)^2))
      
      # Display model summaries
      output$model_summaries2 <- renderPrint({
        cat("Random Forest Model Summary:\n")
        print(rf_model$finalModel)  # For random forest, you can access the final model directly
        cat("RMSE for Random Forest Model on Training Data:", rmse_rf_train, "\n")
        
        # Plot variable importance
        varImpPlot(rf_model$finalModel)
      })
    })
  
  
      # Predict on button click
      observeEvent(input$predict_btn, {
        # Get predictor values from inputs
        predictor_values <- c(predictor1 = input$predictor_value1, predictor2 = input$predictor_value2)
        
        # Predict using Linear Model
        prediction_linear <- predict(linear_model, newdata = predictor_values)
        
        # Predict using Random Forest Model
        prediction_rf <- predict(rf_model, newdata = predictor_values)
        
        # Display predictions
        predictions <- c(Linear_Model = prediction_linear, Random_Forest_Model = prediction_rf)
        output$predictions <- renderPrint(predictions)
      })
})


    
