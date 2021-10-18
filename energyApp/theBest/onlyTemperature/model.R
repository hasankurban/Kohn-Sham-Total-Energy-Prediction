#Data Processing
data <- read.table("anatase.txt", header = T,)
data <- data[,c(1,8)]
names(data) <- c("Temp", "per.atom")
####################################################################
#Simple Linear Model
#Data Visualization
#library(ggplot2)
ggplot(data, aes(x=Temp, per.atom)) +
  geom_point()  +xlab("Temperature") +ylab("Total Energy") + theme_bw()+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x)

#data2 <- data
#data2[,2] <- log(abs(data2[,2]))
#ggplot(data2, aes(x=Temp, per.atom)) +
#  geom_point()  +xlab("Temperature") +ylab("Total Energy") + theme_bw()

#simple linear model
library(tidyverse)
model <- lm(per.atom ~ Temp, data = data2)
summary(model)
summary(model)$coefficient
confint(model)
sigma(model)/mean(data2$per.atom)
####################################################################
#One leave out Cross Validation 
#library(caret)
ncols <- dim(data)[2]
nrows <- dim(data)[1]
my_control <- trainControl(method = "cv",
                           number =nrows,
                           savePredictions = "final",
                           allowParallel = TRUE)
#Models
model_list <- caretList(per.atom~Temp, data=data,
                    trControl = my_control,
                    methodList = c("lm", "rf",
                                   "xgbDART",
                                   "xgbTree","glm","lmStepAIC",
                                   "monmlp","gcvEarth","ppr",
                                   "svmLinear","svmRadial","knn","xgbLinear"),
                    tuneList = NULL,
                    continue_on_fail = FALSE
                    #preProcess = c("center","scale")
                    )


####################################################################
# RMSE
training_results_RMSE <- data.frame(
  LM = min(model_list$lm$results$RMSE),
  RF = min(model_list$rf$results$RMSE),
  XGBDART= min(model_list$xgbDART$results$RMSE),
  LMSTEPAIC= min(model_list$lmStepAIC$results$RMSE),
  MONMLP= min(model_list$monmlp$results$RMSE),
  GVCEARTH = min(model_list$gcvEarth$results$RMSE),
  PPR= min(model_list$ppr$results$RMSE),
  SVMLINEAR= min(model_list$svmLinear$results$RMSE),
  SVMRADIAL= min(model_list$svmRadial$results$RMSE),
  KNN= min(model_list$knn$results$RMSE),
  XGBTREE= min(model_list$xgbTree$results$RMSE),
  XGBLINEAR = min(model_list$xgbLinear$results$RMSE),
  ICR = min(model_list$icr$results$RMSE),
  GAUSPRLINEAR= min(model_list$gaussprLinear$results$RMSE),
  RPART =min(model_list$rpart$results$RMSE)
)
print(training_results_RMSE)
####################################################################
#library(ggarrange)
resamples <- resamples(model_list)
#dotplot(resamples, metric = "RMSE")  
#dotplot(resamples, metric = "MAE")
#dotplot(resamples, metric = "Rsquared")
#densityplot(resamples, metric = "RMSE",auto.key = TRUE, pch = "|")
####################################################################
#sorted_models <- sort(resamples)


#, scales=list(cex=1.7)
bw_RMSE <- bwplot(resamples,metric = "RMSE",col="blue",par.settings=list(box.rectangle=list(col="salmon", fill="salmon",alpha=0.4),box.umbrella=list(col="salmon",alpha=0.4),plot.symbol=list(col="salmon",alpha=0.4)))
print(update(bw_RMSE),position=c(0,0,0.5,1))
bw_MAE <-  bwplot(resamples,metric = "MAE",col="blue", par.settings=list(box.rectangle=list(col="salmon",fill="salmon",alpha=0.4),box.umbrella=list(col="salmon",alpha=0.4),plot.symbol=list(col="salmon",alpha=0.4))) 
print(update(bw_MAE), newpage=FALSE, position=c(0.5,0,1,1))


parallel_RMSE <- parallelplot(resamples, lty=3,  lwd =4,metric = "RMSE")
print(update(parallel_RMSE), newpage=FALSE, position=c(0,0,0.5,1))
parallel_MAE <- parallelplot(resamples ,lty = 3, lwd =4,lend=4, metric = "MAE")
print(update(parallel_MAE), newpage=FALSE, position=c(0.5,0,1,1))

ggarrange(bw_RMSE,parallel_RMSE, bw_MAE, parallel_MAE, bw_R2, parallel_R2, ncol = 2, nrow = 3)




library(corrplot)
library(xtable)
corTable <- modelCor(resamples)
print(xtable(corTable, type = "latex"), file = "corTable.tex")
corrplot(corTable, order="hclust", method = "color",tl.col = "black")
View(modelCor(resamples))


set.seed(222)

# Built the best ML model
ensemble1 <- caretEnsemble(model_list[-c(11,12,2,9,10)], 
                           metric = "RMSE", 
                           trControl = my_control)


ensemble2 <- caretEnsemble(model_list[-c(11,12,2,9,10)], 
                           metric = "RMSE", 
                           trControl = my_control)

summary(ensemble1)
ensemble1s