data3 <- data3[order(data3[,2] ),] 
ggplot(data3, aes(x = Algorithm, y = Weight)) + 
  geom_line(aes(group=2)) + geom_point() + theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())


print(xtable(data3, type = "latex", row.names= FALSE), file = "data3.tex")
