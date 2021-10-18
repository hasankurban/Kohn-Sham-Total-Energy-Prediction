library(caret)
server <- function(input, output) {
  
  
  reactivemy_data<-reactive({
    req(input$file1)
    my_data <- read.table(input$file1$datapath, header=T)
    names(my_data) <- c("Atom Type", "x", "y", "z")
    my_data2 <- my_data #visualization purposes
    my_data[,1] <-as.numeric(as.factor(my_data[,1]))
    req(input$my_temp)
    my_temp <- input$my_temp
    my_data$Temperature <- rep(my_temp,nrow(my_data))
    #my_data2$Temperature <- rep(my_temp,nrow(my_data))#visualization purposes
    #my_data[,5] <-as.numeric(my_data[,5])
    my_data$predictions <-predict(bestModel, newdata = my_data)
    my_data[,1] <- my_data2[,1]
    names(my_data)[6] <- "Kohn-Sham Predictions"
    return(my_data)
  })  
  
  output$mytable = DT::renderDataTable({
    req(input$file1)
    return(DT::datatable(format(reactivemy_data(),digits = 5),  options = list(pageLength = 5), filter = c("top")))
  })
  
  
  output$plot1 <- renderPlot({
    req(input$file1)
    TO.fig.small <- ggpairs(reactivemy_data(), aes(color=as.factor(reactivemy_data()[,1])))+ theme_bw()
    for(i in 1:TO.fig.small$nrow) {
      for(j in 1:TO.fig.small$ncol){
        TO.fig.small[i,j] <- TO.fig.small[i,j] +
          scale_fill_manual(values=c("red3", "steelblue1")) +
          scale_color_manual(values=c("red3", "steelblue1")) 
      }
    }
    TO.fig.small+  theme_bw() + 
      theme(legend.title=element_blank(), panel.grid.major = element_blank(),
            axis.text=element_text(size=8),
            panel.grid.minor = element_blank()
      ) 
  })
  
  # Download the data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".txt", sep = "\t")
    },
    content = function(file) {
      write.table(format(reactivemy_data(),digits = 5), file, sep = "\t", row.names = FALSE)
    }
  )
  
  
}