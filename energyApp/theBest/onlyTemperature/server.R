library(caret)
server <- function(input, output) {
  
  
  
  
  reactiveDF2<-reactive({
  req(input$my_temp)
  df2 <- as.data.frame(input$my_temp)
  names(df2)[1] <- "Temp"
  df2$predictions <-predict(bestModel, newdata =df2)
  return(df2)
  })
  output$mytable2 = DT::renderDataTable({
    req(input$my_temp)
    return(DT::datatable(format(reactiveDF2(),digits = 5)))
  })
  
  ##################
  
  
  reactiveDF<-reactive({
    req(input$my_data)
    df <- read.table(input$my_data$datapath, header=T)
    df$predictions <-predict(bestModel, newdata = df)
    return(df)
    
  })  
  output$mytable = DT::renderDataTable({
    req(input$my_data)
    return(DT::datatable(format(reactiveDF(),digits = 5),  options = list(pageLength = 100), filter = c("top")))
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".txt", sep = "\t")
    },
    content = function(file) {
      write.table(format(reactiveDF(),digits = 5), file, sep = "\t", row.names = FALSE)
    }
  )
}
