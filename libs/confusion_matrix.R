calculate_confusion_matrix_1 <- function(test_set, test_index, chosen_sex) {
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
  table(predicted = y_hat, actual = test_set$sex)
  test_set %>% 
    mutate(y_hat = y_hat) %>%
    group_by(sex) %>% 
    summarize(accuracy = mean(y_hat == sex))
  confusion_matrix_1 <- mean(y == chosen_sex)
  return(confusion_matrix_1)
}