data <- read.table("550K.txt", header =F) 
temperature <- rep(550,268)
data <- cbind(data, temperature)
colnames(data)[1] <- "Atom Type"
colnames(data)[2] <- "x"
colnames(data)[3] <- "y"
colnames(data)[4] <- "z"
colnames(data)[5] <- "Temperature"
data[,1] <- as.numeric(as.factor(data[,1]))
write.table(data, file = "data.txt", sep = "\t",
            row.names = FALSE)


