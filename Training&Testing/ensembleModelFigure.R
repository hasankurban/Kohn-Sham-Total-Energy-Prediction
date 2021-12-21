data3 <- data3[order(data3[,2] ),] 
names(data3)[1]<- "Model"
ggplot(data3, aes(x = Model, y = Weight)) + 
  geom_line(aes(group=2)) + geom_point() + theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  coord_flip()


print(xtable(data3, type = "latex", row.names= FALSE), file = "data3.tex")
