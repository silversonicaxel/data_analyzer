source("utils/init_caret.R")

iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


set.seed(76)

# Split the data
test_index <- createDataPartition(iris$Species, times = 1, p = 0.5, list = FALSE)
train <- iris[-test_index,]
test <- iris[test_index,]

# Function to find the best cutoff and accuracy for a feature
find_best_cutoff <- function(feature) {
  cutoffs <- seq(min(train[[feature]]), max(train[[feature]]), by = 0.1)
  accuracies <- sapply(cutoffs, function(cutoff) {
    y_hat <- ifelse(train[[feature]] > cutoff, "virginica", "versicolor")
    mean(y_hat == train$Species)
  })
  max(accuracies)
}

# Check all features
features <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
accuracies <- sapply(features, find_best_cutoff)
best_feature <- features[which.max(accuracies)]


rangedValues <- seq(range(train[,4])[1], range(train[,4])[2], by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,4]>cutoffs[1], 'virginica', 'versicolor')
mean(y_hat==test$Species)






second_best_feature <- features[order(accuracies, decreasing = TRUE)[2]]


# Define the sequence of cutoffs for both features
length_cutoffs <- seq(min(train$Petal.Length), max(train$Petal.Length), by = 0.1)
width_cutoffs <- seq(min(train$Petal.Width), max(train$Petal.Width), by = 0.1)

# Optimize the combination of both features
best_accuracy <- 0
best_length_cutoff <- 0
best_width_cutoff <- 0

for (length_cutoff in length_cutoffs) {
  for (width_cutoff in width_cutoffs) {
    y_hat_combined <- ifelse(train$Petal.Length > length_cutoff & train$Petal.Width > width_cutoff, "virginica", "versicolor")
    accuracy <- mean(y_hat_combined == train$Species)
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_length_cutoff <- length_cutoff
      best_width_cutoff <- width_cutoff
    }
  }
}

# Apply the best cutoffs to the test data
y_hat_test_combined <- ifelse(test$Petal.Length > best_length_cutoff & test$Petal.Width > best_width_cutoff, "virginica", "versicolor")
combined_test_accuracy <- mean(y_hat_test_combined == test$Species)