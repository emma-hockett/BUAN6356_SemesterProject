# Install and load necessary packages
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
  
  best_cutoff <- 0
  
  for (cut in cutoff) {
    
    yHat <- ifelse(pHat >= cut, 1, 0)
    
    acc <- 100 * mean(yHat == test_data$Attrition)
    
    if (acc > best_acc) {
      
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
  
  TP <- sum(yHat == 1 & test_data$Attrition == 1)
  
  TN <- sum(yHat == 0 & test_data$Attrition == 0)
  
  FP <- sum(yHat == 1 & test_data$Attrition == 0)
  
  FN <- sum(yHat == 0 & test_data$Attrition == 1)
  
  accuracy <- 100 * mean(yHat == test_data$Attrition)
  
  sensitivity <- 100 * (TP / (TP + FN))
  
  specificity <- 100 * (TN / (TN + FP))
  
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
  
}

# List of top predictors
predictors <- c("Performance_Rating", "Overtime", "Monthly_Income", "Job_Level", "Joining_Year", "Business_Travel", "Age", "Years_With_Manager", "Monthly_Rate", "Marital_Status")

# Function to evaluate all combinations of predictors
evaluate_combinations <- function(predictors, train_data, test_data) {
  
  results <- data.frame(Combination = character(), Accuracy = numeric(), Sensitivity = numeric(), Specificity = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(predictors)) {
    
    combs <- combn(predictors, i, simplify = FALSE)
    
    for (comb in combs) {
      
      formula <- as.formula(paste("Attrition ~", paste(comb, collapse = " + ")))
      
      model <- glm(formula, family = binomial, data = train_data)
      
      cutoff_results <- best_cutoff(model, test_data)
      
      eval_results <- eval_model(model, cutoff_results$best_cutoff, test_data)
      
      results <- rbind(results, data.frame(Combination = paste(comb, collapse = " + "), Accuracy = eval_results$accuracy, Sensitivity = eval_results$sensitivity, Specificity = eval_results$specificity))
      
    }
    
  }
  
  return(results)
  
}

# Evaluate all combinations of predictors
results <- evaluate_combinations(predictors, train_data, test_data)

# Sort results by accuracy and display the top combinations
top_results <- results[order(-results$Accuracy), ]
print(head(top_results, 10))  # Display the top 10 combinations based on accuracy
