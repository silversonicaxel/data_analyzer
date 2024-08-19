source("utils/init_data.R")
source("utils/init_data_time.R")

source("utils/init_manipulators.R")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


dat_combined <- data.frame(sex = y, type = x)

# Calculate the proportion of females in the "inclass" group
inclass_female_proportion <- mean(dat_combined$sex[dat_combined$type == "inclass"] == "Female")

# Calculate the proportion of females in the "online" group
online_female_proportion <- mean(dat_combined$sex[dat_combined$type == "online"] == "Female")


# Step 1: Determine the most prevalent sex in each type
most_prevalent_sex <- dat_combined %>%
  group_by(type) %>%
  summarize(most_common_sex = ifelse(mean(sex == "Female") > 0.5, "Female", "Male"))

# Step 2: Create a prediction vector based on the most prevalent sex in each class type
dat_combined <- dat_combined %>%
  left_join(most_prevalent_sex, by = "type") %>%
  mutate(predicted_sex = most_common_sex)

# Step 3: Calculate the accuracy of the prediction
accuracy <- mean(dat_combined$sex == dat_combined$predicted_sex)

library(caret)

# Convert y and y_hat to factors if they are not already
y <- factor(dat_combined$sex, levels = c("Female", "Male"))
y_hat <- factor(dat_combined$predicted_sex, levels = c("Female", "Male"))

# Calculate sensitivity
sensitivity <- sensitivity(y_hat, y)

# Calculate specificity
specificity <- specificity(y_hat, y)


prevalence <- mean(dat_combined$sex == "Female")

print(prevalence)


