# PREDICTIONS
pred_testing_LM <- predict.train(models$lm, newdata = testing[,1:5])
pred_testing_RF <- predict.train(models$rf, newdata = testing[,1:5])
pred_testing_RANDOMGLM <- predict.train(models$randomGLM, newdata = testing[,1:5])
pred_testing_XGBDART <- predict.train(models$xgbDART, newdata = testing[,1:5])
pred_testing_GLM <- predict.train(models$glm, newdata = testing[,1:5])
pred_testing_LMSTEPAIC <- predict.train(models$lmStepAIC, newdata = testing[,1:5])
pred_testing_MONMLP <- predict.train(models$monmlp, newdata = testing[,1:5])
pred_testing_GVCEARTH <- predict.train(models$gcvEarth, newdata = testing[,1:5])
pred_testing_PPR <- predict.train(models$ppr, newdata = testing[,1:5])
pred_testing_SVMLINEAR <- predict.train(models$svmLinear, newdata = testing[,1:5])
pred_testing_SVMRADIAL <- predict.train(models$svmRadial, newdata = testing[,1:5])
pred_testing_KNN <- predict.train(models$knn, newdata = testing[,1:5])
pred_testing_XGBTREE <- predict.train(models$xgbTree, newdata = testing[,1:5])
pred_testing_XGBLINEAR <- predict.train(models$xgbLinear, newdata = testing[,1:5])
pred_ICR <- predict.train(models$icr, newdata = testing[,1:5])
pred_GAUSSLINEAR <- predict.train(models$gaussprLinear, newdata = testing[,1:5])
pred_RPART <- predict.train(models$rpart, newdata = testing[,1:5])
predict_ens1 <- predict(ensemble1, newdata = testing[,1:5])

postResample(pred = pred_testing_LM, obs = testing[,6])
# RMSE
pred_testing_RMSE <- data.frame(ensemble_1 = RMSE(predict_ens1,  testing[,6]),
                        LM = RMSE(pred_LM,  testing[,6]),
                        RF = RMSE(pred_RF,  testing[,6]),
                        XGBDART = RMSE(pred_XGBDART,  testing[,6]),
                        GLM = RMSE(pred_GLM ,  testing[,6]),
                        LMSTEPAIC = RMSE(pred_LMSTEPAIC ,  testing[,6]),
                        MONMLP = RMSE(pred_MONMLP ,  testing[,6]),
                        GVCEARTH = RMSE(pred_GVCEARTH ,  testing[,6]),
                        PPR= RMSE(pred_PPR,  testing[,6]),
                        SVMLINEAR= RMSE(pred_SVMLINEAR,  testing[,6]),
                        SVMRADIAL= RMSE(pred_SVMRADIAL,  testing[,6]),
                        KNN = RMSE(pred_KNN ,  testing[,6]),
                        XGBTREE= RMSE(pred_XGBTREE,  testing[,6]),
                        XGBLINEAR= RMSE(pred_XGBLINEAR,  testing[,6]),
                        ICR = RMSE(pred_ICR,  testing[,6]),
                        GAUSSLINEAR = RMSE(pred_GAUSSLINEAR,  testing[,6]),
                        RPART = RMSE(pred_RPART,  testing[,6])
                        )
print(pred_testing_RMSE)



# RMSE
pred_testing_MAE <- data.frame(ensemble_1 =MAE(predict_ens1,  testing[,6]),
                                LM = MAE(pred_LM,  testing[,6]),
                                RF = MAE(pred_RF,  testing[,6]),
                                XGBDART = MAE(pred_XGBDART,  testing[,6]),
                                GLM = MAE(pred_GLM ,  testing[,6]),
                                LMSTEPAIC = MAE(pred_LMSTEPAIC ,  testing[,6]),
                                MONMLP = MAE(pred_MONMLP ,  testing[,6]),
                                GVCEARTH = MAE(pred_GVCEARTH ,  testing[,6]),
                                PPR= MAE(pred_PPR,  testing[,6]),
                                SVMLINEAR= MAE(pred_SVMLINEAR,  testing[,6]),
                                SVMRADIAL= MAE(pred_SVMRADIAL,  testing[,6]),
                                KNN = MAE(pred_KNN ,  testing[,6]),
                                XGBTREE=MAE(pred_XGBTREE,  testing[,6]),
                                XGBLINEAR= MAE(pred_XGBLINEAR,  testing[,6]),
                                ICR = MAE(pred_ICR,  testing[,6]),
                                GAUSSLINEAR = MAE(pred_GAUSSLINEAR,  testing[,6]),
                                RPART =MAE(pred_RPART,  testing[,6])
)
print(pred_testing_MAE)



pred_testing_postResample <- data.frame(ensemble_1 = postResample(predict_ens1,  testing[,6]),
                                LM =  postResample(pred_LM,  testing[,6]),
                                RF =  postResample(pred_RF,  testing[,6]),
                                XGBDART =  postResample(pred_XGBDART,  testing[,6]),
                                GLM =  postResample(pred_GLM ,  testing[,6]),
                                LMSTEPAIC =  postResample(pred_LMSTEPAIC ,  testing[,6]),
                                MONMLP =  postResample(pred_MONMLP ,  testing[,6]),
                                GVCEARTH =  postResample(pred_GVCEARTH ,  testing[,6]),
                                PPR=  postResample(pred_PPR,  testing[,6]),
                                SVMLINEAR=  postResample(pred_SVMLINEAR,  testing[,6]),
                                SVMRADIAL=  postResample(pred_SVMRADIAL,  testing[,6]),
                                KNN =  postResample(pred_KNN ,  testing[,6]),
                                XGBTREE=  postResample(pred_XGBTREE,  testing[,6]),
                                XGBLINEAR=  postResample(pred_XGBLINEAR,  testing[,6]),
                                ICR = postResample(pred_ICR,  testing[,6]),
                                GAUSSLINEAR = postResample(pred_GAUSSLINEAR,  testing[,6]),
                                RPART = postResample(pred_RPART,  testing[,6]))
print(pred_testing_postResample)[2,]

#Understanding the best models and hybrid models
models$lm$finalModel
models$rf$finalModel
models$xgbLinear$finalModel
