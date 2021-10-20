#@ Dr. Kurban, March 2021, conctact hakurban@gmail.com for any questions.
#IU Bloomington, Computer Science Department
############################## DATA PREPROCESSING #############################################
filelist = list.files(pattern = ".txt")
print(filelist)
datalist = lapply(filelist, function(x)read.table(x, header=F))
finalData =  data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("V1", "V2", "V3", "V4"))), stringsAsFactors=F)
temperature = seq(0,1000,50)
energies <- read.table("../anatase.txt", header=T)
for (i in 1:length(datalist)){
  data <- as.data.frame(datalist[i]) 
  print(dim(data))
  temp.data1 = rep(temperature[i],dim(as.data.frame(datalist[1]))[1])
  final.energy <- energies[i,6]/dim(data)[1]
  temp.energy = rep(final.energy,dim(as.data.frame(datalist[1]))[1])
  print(temperature[i])
  temp.data2 = cbind(data,temp.data1, temp.energy)
  finalData <- rbind(finalData,temp.data2)
}
summary(finalData)
colnames(finalData)[1] <- "Atom Type"
colnames(finalData)[2] <- "x"
colnames(finalData)[3] <- "y"
colnames(finalData)[4] <- "z"
colnames(finalData)[5] <- "Temperature"
colnames(finalData)[6] <- "Energy"

  
