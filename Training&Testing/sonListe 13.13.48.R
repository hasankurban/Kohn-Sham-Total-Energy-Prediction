model.lm <-train(testing[,6] ~., method = "lm", data = testing, trControl = my_control, preProcess = c("center","scale"))
model.lm$finalModel
model.lm$results
#summ(model.lm)
ggplot(varImp(model.lm))
predictions <- predict(model.lm, testingty[,-6])
predictions
mydata[,12]
