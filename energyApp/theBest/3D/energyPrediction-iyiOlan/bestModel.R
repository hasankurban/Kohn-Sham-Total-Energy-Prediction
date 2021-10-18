# Run the application 
shinyApp(ui = ui, server = server)

bestModel <- readRDS("ensemble1.rds")