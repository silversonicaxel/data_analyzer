source("utils/init_data.R")

source("utils/init_manipulators.R")
source("utils/init_caret.R")

source("libs/accuracy.R")


x <- heights$sex
y <- heights$height

set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

accuracy_1 <- calculate_accuracy_1(test_set, test_index)
accuracy_2 <- calculate_accuracy_2(test_set, test_index)
accuracy_3 <- calculate_accuracy_3(test_set, test_index)
accuracy_4 <- calculate_accuracy_4(test_set, test_index)