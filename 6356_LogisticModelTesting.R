################################################################################
#
# Logistic Regression 
# Group 2: BUAN 6356.S01
#
################################################################################

# Install and load necessary packages
install.packages("smotefamily")
install.packages("caret")
library(caret)
library(smotefamily)


##################### Train/ Test Split ########################################
# Creating the testing and training data sets 
set.seed(123)
test_indicies <- createDataPartition(HR_Employee_df$Attrition, p=0.8, list=FALSE)
test_data <- HR_Employee_df[test_indicies, ]
train_data <- HR_Employee_df[-test_indicies, ]

# Adding SMOTE to the training set to counteract the class imbalance of the target variable 
# This should ensure the minority class is more fairly considered while training the data
train_data <- train_data %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, as.numeric)
test_data <- test_data %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, as.numeric)
train_data_smote <- SMOTE(train_data[,-which(names(train_data) == "Attrition")], train_data$Attrition, K=5, dup_size = 2)
train_data$data$class <- as.factor(train_data_smote$data$class)
train_data_smote$syn_data_class <- as.factor(train_data_smote$syn_data_class)
combined_data <- rbind(train_data_smote$data, train_data_smote$syn_data)
combined_data <- data.frame(combined_data, Attrition = combined_data$class)
combined_data$Attrition <- as.factor(combined_data$Attrition)
combined_data$class <- NULL

################## Best Cutoff Function ########################################
# Function to iterate through cutoff values to find the one that produces the highest accuracy scores 
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

####################### Model Evaluation Function ##############################
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


#################### Evaluating Combination Function ###########################

# WARNING: Would not recommend running the evaluate_combinations function!
# This function can take some time to run through because it is testing so many combinations and cutoff values.
# We included the top results in the report, if you want to run this you can it will just take some time to run through.


# List of top predictors to iterate through 
predictors <- c("Overtime", "Performance_Rating", "Percent_Salary_Hike", "Joining_Year", "Age", "Years_At_Company", "Job_Level", "Monthly_Income", "Total_Working_Years", "Business_Travel")

# Function to evaluate all combinations of predictors
evaluate_combinations <- function(predictors, train_data, test_data) {
  
  results <- data.frame(Combination = character(), Accuracy = numeric(), Sensitivity = numeric(), Specificity = numeric(), stringsAsFactors = FALSE)
  
  for (i in 2: length(predictors)) {
    
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
results <- evaluate_combinations(predictors, combined_data, test_data)


# Sort results by accuracy and display the top combinations
top_results <- results[order(-results$Accuracy), ]
print(head(top_results, 10))  # Display the top 10 combinations based on accuracy



#################### Precision and F1 Score Metrics ############################
# After finding the best equations, 3 achieved the same accuracy, sensitivity, and specifity. To try to break the tie testing precision and F1
# To test the Precision or F1-Score you can test the three major equations: 
# Equation 6: "Attrition ~ Performance_Rating + Percent_Salary_Hike + Joining_Year + Years_At_Company + Years_In_Role"
# Equation 7: "Attrition ~ Performance_Rating + Joining_Year + Years_At_Company + Years_In_Role + Stock_Option"
# Equation 8: "Attrition ~ Performance_Rating + Joining_Year + Years_At_Company + Stock_Option + Years_With_Manager"

formulas <- list( "Attrition ~ Performance_Rating + Percent_Salary_Hike + Joining_Year + Years_At_Company + Years_In_Role",
                  "Attrition ~ Performance_Rating + Joining_Year + Years_At_Company + Years_In_Role + Stock_Option",
                  "Attrition ~ Performance_Rating + Joining_Year + Years_At_Company + Stock_Option + Years_With_Manager")

prec_f1 <- data.frame(Formula = character(), Precision = numeric(), F1 = numeric()) 
                
for (f in formulas){
  model <- glm(as.formula(f), family = binomial, data = combined_data)
  cutoff_results <- best_cutoff(model, test_data)
  pHat <- predict(model, test_data, type = "response")
  yHat <- ifelse(pHat >= cutoff_results$best_cutoff, 1, 0)
  TP <- sum(yHat == 1 & test_data$Attrition == 1)
  TN <- sum(yHat == 0 & test_data$Attrition == 0)
  FP <- sum(yHat == 1 & test_data$Attrition == 0)
  FN <- sum(yHat == 0 & test_data$Attrition == 1)
  precision <- 100*(TP/ (TP +FP))
  sensitivity <- 100 * (TP / (TP + FN))
  f1 <- 2*((precision * sensitivity) / (precision + sensitivity))
  
  prec_f1 <- rbind(prec_f1, data.frame(Formula = f, Precision = precision, F1 = f1))
}

prec_f1


####################### Cross Fold Validation Test #############################
# After precision and F1 Scores, the three equations are still tied so testing their stability using k fold validation 
cv_result <- data.frame(Formula = character(), Avg_Accuracy = numeric())

for (f in formulas) {
  set.seed(123)
  train_control <- trainControl(method = "cv", number = 10)
  cv_model <- train(as.formula(f), data = combined_data, method = "glm", family = binomial, trControl = train_control)
  cv_result <- rbind(cv_result, data.frame(Formula = f, Avg_Accuracy = cv_model$results$Accuracy))
}

cv_result
