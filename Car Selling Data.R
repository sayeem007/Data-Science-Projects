rm(list = ls())
library(dplyr)
FinalData<-read.csv("V:/FinalProject.csv",header=TRUE,sep=",")
FinalData


FinalData[FinalData == ""] <- NA
FinalData


colSums(is.na(FinalData))
is.na(FinalData)


FinalData$Buying_Price <- as.factor(FinalData$Buying_Price)
FinalData$Maintenance_Price <- as.factor(FinalData$Maintenance_Price)
FinalData$No_of_Doors <- as.factor(FinalData$No_of_Doors)
FinalData$Person_Capacity <- as.factor(FinalData$Person_Capacity)
FinalData$Size_of_Luggage <- as.factor(FinalData$Size_of_Luggage)
FinalData$Safety <- as.factor(FinalData$Safety)
FinalData$Car_Acceptability <- as.factor(FinalData$Car_Acceptability)
summary(FinalData)
str(FinalData)


#install.packages("ggplot2")
library(ggplot2)
boxplot(FinalData$Buying_Price,FinalData$Maintenance_Price, FinalData$No_of_Doors,FinalData$Person_Capacity, FinalData$Size_of_Luggage,FinalData$Safety,FinalData$Car_Acceptability  ,
        main="Box Plots for FinalData",
        names=c("Buying_Price", "Maintenance_Price","No_of_Doors","Person_Capacity","Size_of_Luggage","Safety","Car_Acceptability"))


str(FinalData)


head(FinalData)
sapply(FinalData, function(x) length(unique(x)))


chi_squared_Price <- chisq.test(FinalData$Buying_Price, FinalData$Car_Acceptability)
chi_squared_Price
chi_squared_MPrice <- chisq.test(FinalData$Maintenance_Price, FinalData$Car_Acceptability)
chi_squared_MPrice
chi_squared_door <- chisq.test(FinalData$No_of_Doors, FinalData$Car_Acceptability)
chi_squared_door
chi_squared_pcapacity <- chisq.test(FinalData$Person_Capacity, FinalData$Car_Acceptability)
chi_squared_pcapacity
chi_squared_luggage <- chisq.test(FinalData$Size_of_Luggage, FinalData$Car_Acceptability)
chi_squared_luggage
chi_squared_Safety <- chisq.test(FinalData$Safety, FinalData$Car_Acceptability)
chi_squared_Safety


FinalData <- FinalData[, c("Buying_Price", "Maintenance_Price", "Person_Capacity", "Size_of_Luggage", "Safety", "Car_Acceptability")]
str(FinalData)
summary(FinalData)


#install.packages("naivebayes")
library(naivebayes)
#install.packages("e1071")
library(e1071)
Selected_attributes <- c("Buying_Price", "Maintenance_Price","Person_Capacity","Size_of_Luggage","Safety","Car_Acceptability")
subset_data <- FinalData[, c(Selected_attributes, "Car_Acceptability")]
naive_bayes_model <- naiveBayes(Car_Acceptability ~ ., data  = subset_data)
naive_bayes_model


set.seed(123) 
train_indices <- sample(nrow(FinalData), 0.8 * nrow(FinalData))
train_data <- FinalData[train_indices, ]
test_data <- FinalData[-train_indices, ]
summary(test_data)
summary(train_data)
NROW(test_data)
NROW(train_data)


naive_bayes_pred <- predict(naive_bayes_model, newdata = test_data)
naive_bayes_pred
accuracy <- sum(naive_bayes_pred == test_data$Car_Acceptability) / nrow(test_data)
paste("Accuracy (Training and Test Data): ", round(accuracy, 2))


#install.packages("caret")
library(caret)
#install.packages("klaR")
library(klaR)
#install.packages("naivebayes")
library(naivebayes)
ctrl <- trainControl(method = "cv", number = 10)
naive_bayes_pred <- train(Car_Acceptability ~ .,
                          data = FinalData, 
                          method = "nb",
                          trControl = ctrl)
naive_bayes_pred


naive_bayes_Con <- predict(naive_bayes_pred, newdata = test_data)
conf_matrix <- confusionMatrix(naive_bayes_Con, test_data$Car_Acceptability)
conf_matrix
recall <- conf_matrix$byClass["Sensitivity"]
precision <- conf_matrix$byClass["Pos Pred Value"]
f_measure <- (2 * precision * recall) / (precision + recall)
paste("Recall: ", recall)
paste("Precision: ", precision)
paste("F-Measure: ", f_measure)
