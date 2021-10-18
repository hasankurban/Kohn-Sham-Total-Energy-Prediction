library(caret)
server <- function(input, output) {
  
  
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
      write.table(reactiveDF(), file, sep = "\t", row.names = FALSE)
    }
  )
  
  
}
