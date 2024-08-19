calculate_accuracy_1 <- function(test_set, test_index) {
  y_hat_1 <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
  accuracy_1 <- mean(y_hat_1 == test_set$sex)
  return(accuracy_1)
}

calculate_accuracy_2 <- function(test_set, test_index) {
  y_hat_2 <- sample(c("Male", "Female"), length(test_index), replace = TRUE) |>
    factor(levels = levels(test_set$sex))
  accuracy_2 <- mean(y_hat_2 == test_set$sex)
  return(accuracy_2)
}

calculate_accuracy_3 <- function(test_set, test_index) {
  y_hat_3 <- ifelse(x > 62, "Male", "Female") |>
    factor(levels = levels(test_set$sex))
  accuracy_3 <- mean(y == y_hat_3)
  return(accuracy_3)
}

calculate_accuracy_4 <- function(test_set, test_index) {
  cutoff <- seq(61, 70)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat_4 <- ifelse(train_set$height > x, "Male", "Female") |>
      factor(levels = levels(test_set$sex))
    mean(y_hat_4 == train_set$sex)
  })
  best_cutoff <- cutoff[which.max(accuracy)]
  y_hat_4 <- ifelse(test_set$height > best_cutoff, "Male", "Female") |>
    factor(levels = levels(test_set$sex))
  y_hat_4 <- factor(y_hat_4)
  accuracy_4 <- mean(y_hat_4 == test_set$sex)
  return(accuracy_4)
}
