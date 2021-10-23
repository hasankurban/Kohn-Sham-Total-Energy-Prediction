#@ Dr. Kurban, July 2021, conctact hakurban@gmail.com for any questions.
################################# PARTION DATA: TRAINING AND TESTING #########################################################
library(caret)
set.seed(555)
finalData[,1] <- as.numeric(as.factor(finalData[,1]))
#Spliting data as training and test set.
indxTrain <- createDataPartition(y = finalData[,1],p = 0.75,list = FALSE)
training <- finalData[indxTrain,]
testing <-finalData[-indxTrain,]

#Checking distibution in original data and partitioned data
prop.table(table(training[,1])) * 100
prop.table(table(testing[,1])) * 100
prop.table(table(finalData[,1])) * 100
#####################################################################################
#Models:
#lm:  linear  regression,
#glm:  Generalized Linear Model, 
#lmStepAIC: Generalized Linear Model with Stepwise Feature Selection
#gcvEarth: Multivariate Adaptive Regression Splines
#ppr: Projection Pursuit Regression
#rf: Random Forest
#xgbDart,XgbTree, xgbLinear: Extreme Gradient  Boosting
#monmlp:Monotone Multi-Layer Perceptron Neural Network
#svmLinear, svmRadial: SVM
#knn: k-nearest neighbor
#rpart: CART
#gaussprLinear: Gaussian Process
#icr: Independent Component Regression
#############################################################
library(caretEnsemble)
library(caret)
set.seed(444)
my_control <- trainControl(method = "cv",
                           number =10,
                           savePredictions = "final",
                           allowParallel = TRUE)

models <- caretList(training[,1:5],training[,6],
                    trControl = my_control,
                    methodList = c("lm","glm","rf","xgbDART","xgbLinear","xgbTree",
                                   "lmStepAIC","monmlp","gcvEarth","ppr",
                                   "svmLinear","svmRadial","knn","icr","gaussprLinear",
                                   "rpart"),
                    tuneList = NULL,
                    continue_on_fail = FALSE, 
                    preProcess = c("center","scale"))
######### Training Error: RMSE   ###########################################
training_results_RMSE <- data.frame(
  LM = min(models$lm$results$RMSE),
  RF = min(models$rf$results$RMSE),
  XGBDART= min(models$xgbDART$results$RMSE),
  GLM = min(models$glm$results$RMSE),
  LMSTEPAIC= min(models$lmStepAIC$results$RMSE),
  MONMLP= min(models$monmlp$results$RMSE),
  GVCEARTH = min(models$gcvEarth$results$RMSE),
  PPR= min(models$ppr$results$RMSE),
  SVMLINEAR= min(models$svmLinear$results$RMSE),
  SVMRADIAL= min(models$svmRadial$results$RMSE),
  KNN= min(models$knn$results$RMSE),
  XGBTREE= min(models$xgbTree$results$RMSE),
  XGBLINEAR = min(models$xgbLinear$results$RMSE),
  ICR = min(models$icr$results$RMSE),
  GAUSPRLINEAR= min(models$gaussprLinear$results$RMSE),
  RPART =min(models$rpart$results$RMSE)
)
print(training_results_RMSE)
####################################################################
resamples <- resamples(models)
#dotplot(resamples, metric = "RMSE")  
#dotplot(resamples, metric = "MAE")
#dotplot(resamples, metric = "Rsquared")
#densityplot(resamples, metric = "RMSE",auto.key = TRUE, pch = "|")

library(ggpubr)

bw_RMSE <- bwplot(resamples,metric = "RMSE",col="blue",par.settings=list(box.rectangle=list(col="salmon", fill="salmon",alpha=0.4),box.umbrella=list(col="salmon",alpha=0.4),plot.symbol=list(col="salmon",alpha=0.4)))
print(update(bw_RMSE),position=c(0,0,0.33,1))

bw_MAE <-  bwplot(resamples,metric = "MAE",col="blue", par.settings=list(box.rectangle=list(col="salmon",fill="salmon",alpha=0.4),box.umbrella=list(col="salmon",alpha=0.4),plot.symbol=list(col="salmon",alpha=0.4))) 
print(update(bw_MAE), newpage=FALSE, position=c(0.33,0,0.66,1))
bw_R2 <-  bwplot(resamples,metric = "Rsquared",col="blue",par.settings=list(box.rectangle=list(col="salmon",fill="salmon",alpha=0.4),box.umbrella=list(col="salmon",alpha=0.4),plot.symbol=list(col="salmon",alpha=0.4)))
print(update(bw_R2),newpage=FALSE, position=c(0.66,0,0.99,1))


parallel_RMSE <- parallelplot(resamples, lty=3,  lwd =4,metric = "RMSE")
print(update(parallel_RMSE), newpage=FALSE, position=c(0,0,0.33,1))
parallel_MAE <- parallelplot(resamples ,lty = 3, lwd =4,lend=4, metric = "MAE")
print(update(parallel_MAE), newpage=FALSE, position=c(0.33,0,0.66,1))
parallel_R2 <-parallelplot(resamples, lty = 3, lwd =4,lend=4,metric = "Rsquared")
print(update(parallel_R2),newpage=FALSE, position=c(0.66,0,0.99,1))

ggarrange(bw_RMSE,parallel_RMSE, bw_MAE, parallel_MAE, bw_R2, parallel_R2, ncol = 2, nrow = 3)


difs <- diff(resamples)
difs_RMSE <- levelplot(difs, what = "differences", metric = "Rsquared", col.regions=rainbow(100))
print(update(difs_RMSE),position=c(0,0,1,0.36))
difs_MAE <- levelplot(difs, what = "differences", metric = "MAE",col.regions=rainbow(100))
print(update(difs_MAE), newpage=FALSE, position=c(0,0.32,1,0.68))
difs_R2 <- levelplot(difs, what = "differences", tl.col="black",metric = "RMSE",col.regions=rainbow(100))
print(update(difs_R2),newpage=FALSE, position=c(0,0.63,1,1))

library(ggpubr)
ggarrange(difs_RMSE, difs_MAE, difs_R2,ncol = 1, nrow = 3)

library(caretEnsemble)
library(corrplot)
library(xtable)
library(caret)
corTable <- modelCor(resamples)
print(xtable(corTable, type = "latex"), file = "corTable.tex")
corrplot(corTable, order="hclust", method = "color",tl.col = "black")
View(modelCor(resamples))


set.seed(222)
  
  ensemble1 <- caretEnsemble(models[-c(1,2,7)], 
                                  metric = "RMSE", 
                                  trControl = my_control)
  
  # Built the best ML model
  ensemble1.RMSE <- caretEnsemble(models[-c(1,2,7)], 
                                  metric = "RMSE", 
                                  trControl = my_control)
  ensemble1.MAE <- caretEnsemble(models[-c(1,2,7)], 
                                 metric = "RMSE", 
                                 trControl = my_control)
  ensemble1.R2 <-caretEnsemble(models[-c(1,2,7)], 
                               metric = "RMSE", 
                               trControl = my_control)
#library(gridExtra)
#library(ggpubr)




summary(ensemble1.RMSE)
plot1 <- plot(ensemble1.RMSE) + coord_flip()
ggplot(ensemble1.RMSE)
varImp(ensemble1.RMSE)
ensemble1.RMSE
 

summary(ensemble1.MAE)
plot2 <- plot(ensemble1.MAE) + coord_flip()
varImp(ensemble1.MAE)
ensemble1.MAE

summary(ensemble1.R2)
plot3 <- plot(ensemble1.R2)+  coord_flip()
varImp(ensemble1.R2)
ensemble1.R2

ggarrange(plot1, plot2, plot3,ncol = 3, nrow = 1)


