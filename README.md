# Final Project

## Brief Description

This Shiny app is designed to provide users with tools for data exploration and modeling based on the US air quality index dataset. The data used in this app is sourced from Kaggle's 'Air Quality Index by State (1980-2022)' dataset. You can find more information and access the data [here](https://www.kaggle.com/datasets/adampq/air-quality-index-by-state-1980-2022)

## Packages Needed

To run this Shiny app, you will need the following R packages:

- library(shiny)
- library(ggplot2)
- library(dplyr)
- library(caret)
- library(randomForest)

## Installing Required Packages

You can install the required packages by running the following R code:

## Install required packages
install.packages(c("shiny", "ggplot2", "dplyr", "caret", "randomForest")) 

## Run the Shiny app from GitHub
shiny::runGitHub('Final_Project_Keren_Vivas', username = "kerenvm26", subdir = "Final_Project")
