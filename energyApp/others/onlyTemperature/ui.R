library(caret)
library(shiny)
library(DT)
library(tidyverse)
library(monmlp)
library(xgboost)
bestModel <- readRDS("ensemble1.rds")
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Kohn-Sham Total Energy Prediction With Cooperative Model Framework"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file ----
      fileInput("my_data", "Upload a .txt with only temperature variable",
                multiple = FALSE,
                accept = ".txt"), 
      
      
      # Button
      downloadButton("downloadData", "Download the Predictions")
    ),
    # Show the table with the predictions
    mainPanel(
      DT::dataTableOutput("mytable")
    )
  )
)
