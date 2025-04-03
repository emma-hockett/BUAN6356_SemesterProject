
# Installing the packages needed
install.packages("caret")
install.packages("gains")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
install.packages("dplyr")
install.packages("smotefamily")
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(dplyr)
library(smotefamily)

# Separating into test and training data 
set.seed(123)
train_indicies <- createDataPartition(HR_Employee_df$Attrition, p=0.8, list=FALSE)
train_data_full <- HR_Employee_df[train_indicies, ]
test_data_full <- HR_Employee_df[-train_indicies, ]
columns_to_exclude <- c("Employee_ID", "Leaving_Year", "Reason", "Relieving_Status")
train_data <- train_data_full %>% select(-all_of(columns_to_exclude))
test_data <- test_data_full %>% select(-all_of(columns_to_exclude))

# Applying SMOTE to training data because of the class imbalance 
train_data <- train_data %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, as.numeric)
train_data_smote <- SMOTE(train_data[, -which(names(train_data) == "Attrition")], train_data$Attrition, K=5, dup_size = 2)
train_data$data$class <- as.factor(train_data_smote$data$class)
train_data_smote$syn_data_class <- as.factor(train_data_smote$syn_data_class)
combined_data <- rbind(train_data_smote$data, train_data_smote$syn_data)
combined_data <- data.frame(combined_data, Attrition = combined_data$class)
combined_data$class <- NULL

# Creating a default tree to look at
set.seed(1)
default_tree <- rpart(Attrition ~., data = combined_data, method = "class")
prp(default_tree, type = 1, extra = 1, under = TRUE)

full_tree <- rpart(Attrition ~ ., data = combined_data, method = "class", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)
printcp(full_tree)

# The minimum error comes when cp is 3.0626e-04 and the number of splits is 40
pruned_tree <- prune(full_tree, cp =  0.00030626)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

# Based on the decision tree 
predicted_class <- predict(pruned_tree, test_data, type = "class")
test_data$Attrition <- factor(test_data$Attrition, levels = c('0', '1'))
confusion_class <- confusionMatrix(predicted_class, test_data$Attrition, positive = "1")
confusion_class

sensitivity <- as.numeric(confusion_class$byClass['Sensitivity'])
precision <- as.numeric(confusion_class$byClass['Pos Pred Value'])
f1 <- 2*((precision * sensitivity) / (precision + sensitivity))


# Accuracy: 0.984
# Sensitivity: 0.9534
# Specificity: 0.9932
# Precision: 0.9769
# F1-Score: 0.9650



# Approximately 24% of the data falls into the target class, so the use that as the cutoff 
predicted_prob <- predict(pruned_tree, test_data, type= 'prob')
predicted_class_prob <- factor(ifelse(predicted_prob[,2]>0.24, '1', '0'), levels = c('0', '1'))
confusion_prob <- confusionMatrix(predicted_class_prob, test_data$Attrition, positive = '1')
confusion_prob

sensitivity <- as.numeric(confusion_prob$byClass['Sensitivity'])
precision <- as.numeric(confusion_prob$byClass['Pos Pred Value'])
f1 <- 2*((precision * sensitivity) / (precision + sensitivity))

# Accuracy: 0.984
# Sensitivity: 0.9534
# Specificity: 0.9932
# Precision: 0.9769
# F1-Score: 0.9650


# Plotting the ROC curve. Areas under the curve: 0.98
roc_curve <- roc(test_data$Attrition, predicted_prob[,2])
plot(roc_curve)
auc(roc_curve)

