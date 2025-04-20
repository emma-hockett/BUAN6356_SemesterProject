################################################################################
#
# Logistic Regression 
# Group 2: BUAN 6356.S01
#
################################################################################

# Install and load necessary packages
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
# After finding the best equations, 3 achieved the same accuracy, sensitivity, and specifity. To try and break the tie testinf precision and F1
formula <- "Attrition ~ Performance_Rating + Joining_Year + Years_At_Company + Stock_Option + Years_With_Manager"
model <- glm(formula, family = binomial, data = combined_data)
cutoff_results <- best_cutoff(model, test_data)
pHat <- predict(model, test_data, type = "response")
yHat <- ifelse(pHat >= cutoff_results$best_cutoff, 1, 0)
TP <- sum(yHat == 1 & test_data$Attrition == 1)
TN <- sum(yHat == 0 & test_data$Attrition == 0)
FP <- sum(yHat == 1 & test_data$Attrition == 0)
FN <- sum(yHat == 0 & test_data$Attrition == 1)
accuracy <- 100 * mean(yHat == test_data$Attrition)
sensitivity <- 100 * (TP / (TP + FN))
specificity <- 100 * (TN / (TN + FP))
precision <- 100*(TP/ (TP +FP))
precision
f1 <- 2*((precision * sensitivity) / (precision + sensitivity))
f1


####################### Cross Fold Validation Test #############################
# After precision and F1 Scores, the three equations are still tied so testing their stability using k fold validation 

formula <- "Attrition ~ Performance_Rating + Joining_Year + Years_At_Company + Percent_Salary_Hike + Years_In_Role"
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(as.formula(formula), data = combined_data, method = "glm", family = binomial, trControl = train_control)
cv_model$results

