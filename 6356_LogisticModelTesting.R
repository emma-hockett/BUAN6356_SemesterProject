
# Using the Caret Package because it allows us to split up the test and train data 
# while considering the class imbalance in the target variable Attrition
install.packages("caret")
library(caret)

# Creating the testing and training data sets 
set.seed(123)
test_indicies <- createDataPartition(HR_Employee_df$Attrition, p=0.8, list=FALSE)
test_data <- HR_Employee_df[test_indicies, ]
train_data <- HR_Employee_df[-test_indicies, ]


# Function to iterate through cutoff values and find the one with the highest 
# accuracy for a given model.
best_cutoff <- function(model, test_data, cutoff = seq(0.1, 0.9, by = 0.05)) {
  pHat <- predict(model, test_data, type = "response")
  
  best_acc <- 0
  best_cutoff <-0
  
  for (cut in cutoff) {
    yHat <- ifelse(pHat >= cut, 1, 0)
    acc <- 100* mean(yHat == test_data$Attrition)
    
    if( acc > best_acc) {
      best_acc <- acc
      best_cutoff <- cut
    }
  }
  return(list(best_cutoff = best_cutoff, best_accuracy = best_acc))
}

# Function to find the accuracy, sensitivity, and specificity of the model
eval_model <- function(model, cutoff, test_data) {
  pHat <- predict(model, test_data, type = "response")
  yHat <- ifelse(pHat >= cutoff, 1, 0)
  
  TP <- ifelse(yHat == 1 & test_data$Attrition == 1, 1, 0)
  TN <- ifelse(yHat == 0 & test_data$Attrition == 0, 1, 0)
  
  accuracy <- 100* mean(yHat == test_data$Attrition)
  sensitivity <- 100* (sum(TP) / sum(test_data$Attrition == 1))
  specificity <- 100* (sum(TN) / sum(test_data$Attrition == 0))
  
  sprintf("Accuracy = %f%%", accuracy)
  sprintf("Sensitivity = %f%%", sensitivity)
  sprintf("Specificity = %f%%", specificity)
  
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
}

# Testing the strongest predictors based on the correlation measures 
# Predictors: Total Working Years, Age, Monthly Income, Overtime, and Performance Rating
logModel<- glm(Attrition ~ Total_Working_Years + Age + Monthly_Income + Overtime + Performance_Rating, family = binomial, data = train_data) 
summary(logModel)

# Based on the model finding the most accurate cutoff value and then testing the 
# goodness of fit measures.
cutoff_results <- best_cutoff(logModel, test_data)
eval_model(logModel, cutoff_results$best_cutoff, test_data)




