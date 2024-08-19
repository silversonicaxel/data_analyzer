source("libs/confusion_matrix.R")

confusion_matrix_1 <- calculate_confusion_matrix_1(test_set, test_index, "Male")
print(confusion_matrix_1)