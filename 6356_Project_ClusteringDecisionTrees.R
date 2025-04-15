################################################################################
# 
# Clustering Prior to Decision Trees 
# Group 2: BUAN 6320.S01
#
################################################################################

library(caret)
library(rpart)
library(cluster)
library(dplyr)
library(factoextra)
library(smotefamily)

suppressWarnings(RNGversion("3.5.3"))

# Turning Marital status from ordinal to dummy variable 
Employee_Data <- HR_Employee_df
Employee_Data$Marital_Stat_Single <- ifelse(Employee_Data$Marital_Status == 0, 1, 0)
Employee_Data$Marital_Stat_Married <- ifelse(Employee_Data$Marital_Status == 1, 1, 0)
Employee_Data$Marital_Status <- NULL
columns_to_exclude <- c("Employee_ID", "Leaving_Year", "Reason", "Relieving_Status")
Employee_Data <- HR_Employee_df %>% select(-all_of(columns_to_exclude))

# Separating the data into training and testing 
set.seed(123)
train_indicies <- createDataPartition(Employee_Data$Attrition, p = 0.8, list = FALSE)
train_data <- Employee_Data[train_indicies, ]
test_data <- Employee_Data[-train_indicies, ]

# Adding Synthetic data to the training set to battle the class imbalance
train_data <- train_data %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, as.numeric)
train_data_smote <- SMOTE(train_data[, -which(names(train_data) == "Attrition")], train_data$Attrition, K=5, dup_size = 2)
combined_data <- rbind(train_data_smote$data, train_data_smote$syn_data)
combined_data$Attrition <- as.factor(c(train_data_smote$data$class, train_data_smote$syn_data$class))
combined_data$class <- NULL


# Preparing the dataset for clustering
attrition_labels <- combined_data$Attrition
cluster_data <- combined_data %>% select(-Attrition)
cluster_data1 <- scale(cluster_data[,1:40])


############### Calculating Silhouette Scores of Ks ############################
# Testing various values of k for k means clustering
silhouette_scores <- numeric()

for (k in 2:10){
  set.seed(123)
  kmeans_result <- kmeans(cluster_data1, centers = k, nstart = 25)
  
  sil <- silhouette(kmeans_result$cluster, dist(cluster_data1))
  
  silhouette_scores[k] <- mean(sil[,3])
  
  cat("Average silhouette for k = ", k, ":", silhouette_scores[k], "\n")
}

# k = 2 is the best option
# Average silhouette for k = 2: 0.1295244
# Average silhouette for k = 3: 0.06245679
# Average silhouette for k = 4: 0.05413377
# Average silhouette for k = 5: 0.05536875
# Average silhouette for k = 6: 0.0612255
# Average silhouette for k = 7: 0.07399049
# Average silhouette for k = 8: 0.08024458
# Average silhouette for k = 9: 0.07514228
# Average silhouette for k =10: 0.07273328


# Classifying each of the records into its correct cluster
set.seed(123)
k_means_result<- kmeans(cluster_data1, centers = 2, nstart = 25)
combined_data$Cluster <- k_means_result$cluster
cluster1 <- combined_data[combined_data$Cluster == 1, ]
cluster2 <- combined_data[combined_data$Cluster == 2, ]


######################### Decision Trees #######################################
# Cluster 1 Decision Tree
set.seed(1)
full_tree_cluster1 <- rpart(Attrition ~., data = cluster1, method = "class")
printcp(full_tree_cluster1)
pruned_tree_cluster1 <- prune(full_tree_cluster1, cp = 0.01)


# Cluster 2 Decision Tree
set.seed(1)
full_tree_cluster2 <- rpart(Attrition ~., data = cluster2, method = "class")
printcp(full_tree_cluster2)
pruned_tree_cluster2 <- prune(full_tree_cluster2, cp = 0.01)


################## Assigning Test data to Clusters #############################
# Assigning the test data to a cluster to know which decision tree to use 
test_data <- test_data %>% mutate_if(is.character, as,factor) %>% mutate_if(is.factor, as.numeric)
test_data_wo_Attrition <- test_data %>% select(-Attrition)
test_data_scaled <- scale(test_data_wo_Attrition[,1:40])

test_clusters <- apply(test_data_scaled, 1, function(x) {
  dist_to_centroid1 <- sum((x - k_means_result$centers[1,])^2)
  dist_to_centroid2 <- sum((x - k_means_result$centers[2,])^2)
  
  if(dist_to_centroid1 < dist_to_centroid2){
    return(1)
  } else {
    return(2)
  }
})

test_data$Cluster <- test_clusters
cluster1_test_data <- test_data[test_data$Cluster == 1,]
cluster2_test_data <- test_data[test_data$Cluster == 2,]


################ Making Predictions Through Decision Trees #####################
# Making the predictions for the class based on the correct decision tree 
cluster1_predictions <- predict(pruned_tree_cluster1, cluster1_test_data, type = "class")
cluster2_predictions <- predict(pruned_tree_cluster2, cluster2_test_data, type = "class")

combined_predictions <- c(cluster1_predictions, cluster2_predictions)
cluster1_test_data$Predictions <- cluster1_predictions
cluster2_test_data$Predictions <- cluster2_predictions

combined_test_data <- rbind(cluster1_test_data, cluster2_test_data)

combined_predictions <- factor(combined_predictions, levels = c("0", "1"))
combined_test_data$Attrition <- factor(combined_test_data$Attrition, levels = c("0", "1"))

confusionMatrix(combined_predictions, combined_test_data$Attrition, positive = "1")

# Accuracy: 0.9613
# Sensitivity: 0.8682
# Specificity: 0.9893
# Precision: 0.9609

