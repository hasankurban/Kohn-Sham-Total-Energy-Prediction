#@Dr. Kurban, Kohn-Sham Total Energy Prediction
#data visualization
#IU Bloomington, Computer Science Department
library(ggplot2)
library(GGally)
require(reshape2)

finalDataVis <- finalData
finalDataVis[,1] <- as.factor(finalData[,1])
  TO.fig.small <- ggpairs(finalDataVis, aes(color=finalDataVis[,1]), upper = list(continuous = wrap("cor", size = 7)))+ theme_bw()
  for(i in 1:TO.fig.small$nrow) {
    for(j in 1:TO.fig.small$ncol){
      TO.fig.small[i,j] <- TO.fig.small[i,j] +
        scale_fill_manual(values=c("red3", "steelblue1")) +
        scale_color_manual(values=c("red3", "steelblue1")) 
    }
  }
  TO.fig.small+  theme_bw() +  theme_grey(base_size = 19)+
    theme(legend.title=element_blank(), panel.grid.major = element_blank(),
          axis.text=element_text(size=12),
          panel.grid.minor = element_blank()
    ) 