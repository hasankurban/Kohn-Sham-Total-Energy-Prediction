library(caret)
library(shiny)
library(DT)
library(tidyverse)
library(monmlp)
library(xgboost)
library(caretEnsemble)
library(ggplot2)
library(GGally)
require(reshape2)
bestModel <- readRDS("ensemble1.rds")
ui <- fluidPage(
  # title
  titlePanel("Kohn-Sham Total Energy Prediction With Cooperative Model Framework: TiO2"),
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      numericInput("my_temp", "Temperature in K:", 0,min = -10000, max = 10000),
      # Input: Select a file ----
      fileInput("file1", "Upload a .txt file: 3D location data: Atom type, x, y, z",
                multiple = FALSE,
                accept = ".txt"), 
      
      
      # Button
      downloadButton("downloadData", "Download the Predictions")
    ),
    # Show the table with the predictions
    mainPanel(
      plotOutput("plot1", click = "plot_click"),
      DT::dataTableOutput("mytable")
    )
  ),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  h5(div(HTML("An Efficient and Novel Approach for Predicting Kohn-Sham Total Energy: Bootstrapping a Cooperative Model Framework with Minimal Viable Theoretical Data 
              <em> H. Kurban, M. Kurban, M. M. Dalkilic</em>  Computer Science Department, India University Bloomington, IN, 47408, USA"))),
  
)