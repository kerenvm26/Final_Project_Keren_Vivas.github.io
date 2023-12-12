
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
})
