data3<-read.csv(file.choose(),header=T)
View(data3)

colnames(data3)[which(colnames(data3) == "Target")] <- "Grade"
View(data3)

# Print the rows where Worth_the_price is 45
print(data3[data3$Worth_the_price == "45", ])
#all the 4 observations are 'no' so removing them doesnt make much difference

# Remove the rows where Worth_the_price is 45
data3 <- data3[data3$Worth_the_price != "45", ]

# Replace 'yes' with 1 and 'no' with 0 for categorical variables
data3$Worth_the_price <- ifelse(data3$Worth_the_price == 'yes', 1, 0)
data3$Vegan_options <- ifelse(data3$Vegan_options == 'yes', 1, 0)
data3$Smoking_area <- ifelse(data3$Smoking_area == 'yes', 1, 0)
data3$Parking <- ifelse(data3$Parking == 'yes', 1, 0)
data3$Pet_friendly <- ifelse(data3$Pet_friendly == 'yes', 1, 0)

# Print the updated dataset
print(data3)
View(data3)



#correlation matrix
# Extract 'Grade' as a vector
grade <- data3[, ncol(data3)]
# Select all features excluding the last column (assuming 'Grade')
features <- data3[, 1:(ncol(data3) - 1)]
# Calculate correlation between 'Grade' and features using Spearman's rank
correlation_matrix <- cor(grade, features, method = "spearman")
# Print the correlation matrix
print(correlation_matrix)




#MODEL - LOGISTIC REGRESSION
# Install required packages if not already installed
install.packages("caret")
library(caret)  # Load caret package for model building and evaluation

# Train-test split (recommended for more robust evaluation)
set.seed(123)  # Set random seed for reproducibility
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
split_index <- createDataPartition(data3$Grade, p = 0.8, list = FALSE)
train_data <- data3[split_index,]
test_data <- data3[-split_index,]

# Separate features and target variable within the split data (optional, but improves clarity)
train_features <- train_data[, 1:(ncol(train_data) - 1)]
train_grade <- train_data$Grade
test_features <- test_data[, 1:(ncol(test_data) - 1)]
test_grade <- test_data$Grade

# Build and train the model (Logistic Regression in this example)
fit <- train(x = train_features, y = train_grade, method = "glm", family = "binomial", trControl = ctrl)

# Make predictions on unseen data (test set)
predictions <- predict(fit, newdata = test_features, type = "raw")
print(predictions)
predicted_class <- ifelse(predictions > 0.5, 1, 0)  # Threshold for class prediction
print(predicted_class)

# Evaluate model performance (accuracy in this example)
confusion_matrix <- table(test_grade, predicted_class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Model accuracy on test set:", accuracy))

# Explore other metrics (optional)
library(yardstick)  # Install if not already installed
sensitivity <- sensitivity(test_grade, predicted_class)
specificity <- specificity(test_grade, predicted_class)
print(paste("Sensitivity (Recall) on test set:", sensitivity))
print(paste("Specificity on test set:", specificity))
