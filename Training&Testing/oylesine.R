
# PREDICTIONS
pred_LM <- predict.train(models$lm, newdata = training[,1:5])
pred_RF <- predict.train(models$rf, newdata = training[,1:5])
pred_XGBDART <- predict.train(models$xgbDART, newdata = training[,1:5])
pred_GLM <- predict.train(models$glm, newdata = training[,1:5])
pred_LMSTEPAIC <- predict.train(models$lmStepAIC, newdata = training[,1:5])
pred_MONMLP <- predict.train(models$monmlp, newdata = training[,1:5])
pred_GVCEARTH <- predict.train(models$gcvEarth, newdata = training[,1:5])
pred_PPR <- predict.train(models$ppr, newdata = training[,1:5])
pred_SVMLINEAR <- predict.train(models$svmLinear, newdata = training[,1:5])
pred_SVMRADIAL <- predict.train(models$svmRadial, newdata = training[,1:5])
pred_KNN <- predict.train(models$knn, newdata = training[,1:5])
pred_XGBTREE <- predict.train(models$xgbTree, newdata = training[,1:5])
pred_XGBLINEAR <- predict.train(models$xgbLinear, newdata = training[,1:5])
pred_ICR <- predict.train(models$icr, newdata = training[,1:5])
pred_GAUSSLINEAR <- predict.train(models$gaussprLinear, newdata = training[,1:5])
pred_RPART <- predict.train(models$rpart, newdata = training[,1:5])



plot(training[,6], pred_XGBLINEAR,
     main = "Extreme Gradient Boosting", xlab="True Values", ylab= "Prediction", pch=17, col=c("red"))
abline(lm(pred_XGBLINEAR~training[,6]))


#Now we can test the robustness 
#of the glmnet model by testing it on test_set data set, and regressing the predicted and measured values.
plot(training[,6], pred_KNN,
     main = "K-Nearest Neighbor", xlab="Measured", ylab= "Predicted", pch=17, col=c("red"))
abline(lm(pred_KNN~training[,6]))


#two models
plot(training[,6], pred_XGBLINEAR,
     pch=17, col=c("red"), ylab = "Predicted", xlab = "Measured")
points(training[,6], pred_PPR, pch=20, col=("blue"))
abline(lm(pred_XGBLINEAR~training[,6]), lwd=1, col="red")
abline(lm(pred_PPR~training[,6]), lwd=1, col="blue")
legend(14,9, legend=c("Extreme Gradient Boosting", "PPR"),
       col=c("red", "blue"), lty=1, cex=0.8)



predict_ens1 <- predict(ensemble3, newdata = training[,1:5])
# RMSE
#ensemble2 = RMSE(predict_ens2,  training[,6]),
pred_RMSE <- data.frame(ensemble1 = RMSE(predict_ens1,  training[,6]),
                        LM = RMSE(pred_LM,  training[,6]),
                        RF = RMSE(pred_RF,  training[,6]),
                        XGBDART = RMSE(pred_XGBDART,  training[,6]),
                        GLM = RMSE(pred_GLM ,  training[,6]),
                        LMSTEPAIC = RMSE(pred_LMSTEPAIC ,  training[,6]),
                        MONMLP = RMSE(pred_MONMLP ,  training[,6]),
                        GVCEARTH = RMSE(pred_GVCEARTH ,  training[,6]),
                        PPR= RMSE(pred_PPR,  training[,6]),
                        SVMLINEAR= RMSE(pred_SVMLINEAR,  training[,6]),
                        SVMRADIAL= RMSE(pred_SVMRADIAL,  training[,6]),
                        KNN = RMSE(pred_KNN ,  training[,6]),
                        XGBTREE= RMSE(pred_XGBTREE,  training[,6]),
                        XGBLINEAR= RMSE(pred_XGBLINEAR,  training[,6]),
                        ICR = RMSE(pred_ICR,  training[,6]),
                        GAUSSLINEAR = RMSE(pred_GAUSSLINEAR,  training[,6]),
                        RPART = RMSE(pred_RPART,  training[,6])
)
print(pred_RMSE)

set.seed(123)
xgbTree_model <- train( training[,1:5],
                        training[,6],
                        trControl = my_control,
                        method = "xgbLinear",
                        metric = "RMSE",
                        preProcess = c("center","scale"),
                        importance = TRUE)
plot(varImp(xgbTree_model))
varImp(ensemble_1)
xgbTree_model$finalModel
plot(varImp(ensemble1))


ppr_model <- train( training[,1:5],
                    training[,6],
                    trControl = my_control,
                    method = "ppr",
                    metric = "RMSE",
                    preProcess = c("center","scale"),
                    importance = TRUE)
plot(varImp(ppr_model))
ppr_model$finalModel



pred_cor <- data.frame(ensemble3 = cor(predict_ens1,  training[,6]),
                       LM = cor(pred_LM,  training[,6]),
                       RF= cor(pred_RF,  training[,6]),
                       XGBDART = cor(pred_XGBDART,  training[,6]),
                       GLM= cor(pred_GLM,  training[,6]),
                       LMSTEPAIC= cor(pred_LMSTEPAIC,  training[,6]),
                       MONMLP= cor(pred_MONMLP,  training[,6]),
                       GVCEARTH= cor(pred_GVCEARTH,  training[,6]),
                       PPR = cor(pred_PPR,  training[,6]),
                       SVMLINEAR= cor(pred_SVMLINEAR,  training[,6]),
                       SVMRADIAL = cor(pred_SVMRADIAL,  training[,6]),
                       KNN= cor(pred_KNN,  training[,6]),
                       XGBTREE= cor(pred_XGBTREE,  training[,6]),
                       XGBLINEAR= cor(pred_XGBLINEAR,  training[,6]),
                       ICR = cor(pred_ICR,  training[,6]),
                       GAUSSLINEAR = cor(pred_GAUSSLINEAR,  training[,6]),
                       RPART = cor(pred_RPART,  training[,6])
)

library(xtable)
library(corrplot)
print(xtable(pred_cor, type = "latex"), file = "corOutput.tex")
corrplot(pred_cor, type="upper", order="hclust")