temperature <- as.data.frame(seq(0,2000,50))
names(temperature)[1] <-"Temp"
write.table(temperature, file = "test2.txt", sep = "\t",
            row.names = FALSE)


