
# Installing the packages needed
install.packages("caret")
install.packages("gains")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)

# Separating into test and training data while accounting for the class imbalance
set.seed(123)
test_indicies <- createDataPartition(HR_Employee_df$Attrition, p=0.8, list=FALSE)
test_data <- HR_Employee_df[test_indicies, ]
train_data <- HR_Employee_df[-test_indicies, ]

# Creating a default tree to look at
set.seed(1)
default_tree <- rpart(Attrition ~., data = train_data, method = "class")
prp(default_tree, type = 1, extra = 1, under = TRUE)

full_tree <- rpart(Attrition ~ ., data = train_data, method = "class", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)
printcp(full_tree)

# The minimum error comes when cp = 0.00297619
pruned_tree <- prune(full_tree, cp = 0.00297619)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

# Based on the decision tree 
predicted_class <- predict(pruned_tree, test_data, type = "class")
test_data$Attrition <- factor(test_data$Attrition, levels = c('0', '1'))
confusionMatrix(predicted_class, test_data$Attrition, positive = "1")

# Accuracy: 0.9733
# Sensitivity: 0.9175
# Specificity: 0.9909

# Approximately 24% of the data falls into the target class, so the use that as the cutoff 
predicted_prob <- predict(pruned_tree, test_data, type= 'prob')
predicted_class_prob <- factor(ifelse(predicted_prob[,2]>0.24, '1', '0'), levels = c('0', '1'))
confusionMatrix(predicted_class_prob, test_data$Attrition, positive = '1')

# Accuracy: 0.9713
# Sensitivity: 0.9202
# Specificity: 0.9875

# Plotting the ROC curve. Areas under the curve: 0.982
roc_curve <- roc(test_data$Attrition, predicted_prob[,2])
plot(roc_curve)
auc(roc_curve)