data2<-read.csv(file.choose(),header=T)
View(data2)
data2 <- subset(data2, select = -c(Program.Identifier,Address,Zip.Code,Phone,Inspection.Business.Name,Business_ID,Inspection_Serial_Num,Violation_Record_ID))
View(data2)


#COLUMN - DESCRIPTION 
# Install the tidyr package if not already installed
install.packages("tidyr")
# Load the tidyr package
library(tidyr)
# Split Description column into Type and Risk Category columns
data2 <- separate(data2, Description, into = c("Type", "Risk_Category"), sep = " - ")
# View the updated dataset
head(data2)
data2$Risk_Category <- gsub("Risk II", "Risk Category II", data2$Risk_Category)
library(dplyr)
data2$Type <- recode(data2$Type,
                     "Bakery-no seating" = "Bakery",
                     "Bed and Breakfast" = "Bread&Breakfast",
                     "Grocery Store-no seating" = "Grocery Store")
# Subset the data for "no permanent plumbing"
subset_data <- data2[data2$Risk_Category == "no permanent plumbing", ]
# Replace "no permanent plumbing" with "Risk Category II"
data2$Risk_Category <- gsub("no permanent plumbing", "Risk Category II", data2$Risk_Category)




#COLUMN - CITY
# Remove all spaces between words in city names
data2$City <- gsub("\\s+", "", trimws(tolower(data2$City)))
# Replace "(none)" with "Unknown" in the City column
data2$City <- gsub("\\(none\\)", "Unknown", data2$City)


#COLUMN -  INSPECTION SCORE
#remove the rows which have Inspection score as NA value and less than 0
data2 <- data2[!is.na(data2$Inspection.Score), ]
data2 <- data2[data2$Inspection.Score >= 0, ]


#COLUMN - ISNPECTION RESULT
# Install the dplyr package
install.packages("dplyr")
library(dplyr)
data2$Inspection.Result <- dplyr::recode(data2$Inspection.Result,
                                         "Baseline Data" = "Incomplete",
                                         "Confirmed" = "Satisfactory",
                                         "Exchange information" = "Incomplete",
                                         "In Compliance" = "Complete",
                                         "Incomplete" = "Incomplete",
                                         "Increased Knowledge" = "Incomplete",
                                         "Needs Assessment" = "Incomplete",
                                         "No Longer At Location" = "Incomplete",
                                         "Not Accessible" = "Incomplete",
                                         "Not Applicable" = "Incomplete",
                                         "Not Confirmed" = "Incomplete",
                                         "Not In Compliance" = "Unsatisfactory",
                                         "Not Permitted" = "Unsatisfactory",
                                         "Not Ready For Inspection" = "Incomplete",
                                         "Not Tested" = "Incomplete",
                                         "Out of Business" = "Incomplete",
                                         "Satisfactory" = "Satisfactory",
                                         "Unsatisfactory" = "Unsatisfactory"
)
# Remove rows with empty Inspection.Result
data2 <- data2[data2$Inspection.Result != "", ]




#COLUMN - INSPECTION CLOSED BUSINESS
data2 <- data2[data2$Inspection.Closed.Business != "", ]




#COLUMN - VIOLATION TYPE
# Assuming 'data2' is your dataframe
data2$Violation.Type[data2$Violation.Type == ""] <- "WHITE"





#COLUMN - GRADE
sum(is.na(data2$Grade))
#since we have avoided this mapping....now there are 
#some NA values present in data2$Grade which should be removed
data2 <- data2[!is.na(data2$Grade), ]
sum(is.na(data2$Grade))




#CREATING ML MODEL
#creating a subset from data2
mydata<- data2[, c("Type","Risk_Category", "Inspection.Score", "Inspection.Result", "Inspection.Closed.Business", "Violation.Type", "Violation.Points", "Grade")]
View(mydata)
#risk category i->3,ii->2,iii->1
# Transform Risk_Category column
mydata$Risk_Category <- as.numeric(factor(mydata$Risk_Category, levels = c("Risk Category I", "Risk Category II", "Risk Category III"), labels = c(3,2,1)))

#Satisfactory=1, Complete=2, Incomplete=3, Unsatisfactory=4
# Transform Inspection.Result column
mydata$Inspection.Result <- as.numeric(factor(mydata$Inspection.Result, levels = c("Satisfactory", "Complete", "Incomplete", "Unsatisfactory"), labels = c(1, 2, 3, 4)))

#true=1,false=0
# Transform Inspection.Closed.Business column
mydata$Inspection.Closed.Business <- as.integer(mydata$Inspection.Closed.Business == "true")


#EXPLANATION :
#In the code provided, we used a different approach to transform the columns compared 
#to the if else statement. The factor function is used to create a categorical factor 
#variable, and then as.numeric is used to convert this factor variable to numeric, 
#effectively assigning numeric values to the levels of the factor.
#Using factor and as.numeric is a more concise and efficient way to map 
#categorical values to numeric values when you have a fixed set of categories 
#and their corresponding numeric values. This approach is especially useful when 
#dealing with categorical variables that have a natural order or hierarchy, as 
#in the case of risk categories or inspection results.

unique_values <- unique(mydata$Violation.Type)
value_types <- sapply(unique_values, class)
result <- data.frame(Value = unique_values, Type = value_types)
print(result)

#Blue=2, Red=3, White=1
# Replace values in the Violation.Type column
mydata$Violation.Type <- ifelse(mydata$Violation.Type == "BLUE", 2,
                                ifelse(mydata$Violation.Type == "RED", 3,
                                       ifelse(mydata$Violation.Type == "WHITE", 1, mydata$Violation.Type)))

# Check the updated values
table(mydata$Violation.Type)



# Seating categories
# Seating > 250 = 5, Seating 0-12 = 1, Seating 13-50 = 2, Seating 51-150 = 3, Seating 151-250 = 4
mydata$Type <- ifelse(mydata$Type == "Seating > 250", 5,
                      ifelse(mydata$Type == "Seating 0-12", 1,
                             ifelse(mydata$Type == "Seating 13-50", 2,
                                    ifelse(mydata$Type == "Seating 51-150", 3,
                                           ifelse(mydata$Type == "Seating 151-250", 4, NA)))))




#FINAL CORRELATION-without mapping
cols3 <- c("Risk_Category", "Inspection.Score", "Inspection.Result", "Inspection.Closed.Business", "Violation.Type", "Violation.Points","Type")

# Convert columns to numeric if they are not already numeric
mydata[, cols3] <- lapply(mydata[, cols3], as.numeric)

# Calculate correlation matrix
correlation_matrix3<- cor(mydata[, cols3], mydata$Grade)

# Print correlation matrix
print(correlation_matrix3)




#MODEL_RANDOM FORESTS
# Install required packages if not already installed
install.packages("caret")
install.packages("randomForest")

library(caret)  # Load caret package for model building and evaluation
library(randomForest)
# Split the dataset into training and testing sets
# Load the randomForest package
library(randomForest)
library(pROC)
# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- sample(1:2, nrow(mydata), replace = TRUE, prob = c(0.8, 0.2))
train_data <- mydata[trainIndex == 1, ]
test_data <- mydata[trainIndex == 2, ]

# Convert Grade to factor
train_data$Grade <- as.factor(train_data$Grade)
test_data$Grade <- as.factor(test_data$Grade)

# Calculate class weights
class_weights <- table(train_data$Grade) / nrow(train_data)
# Increase weight for class 4
class_weights_adjusted <- class_weights
#---->attempt 3
class_weights_adjusted["3"] <- 0.15  # Update weight for class 3
class_weights_adjusted["4"] <- class_weights_adjusted["4"] * 10  # Update weight for class 4


# Train the model using Random Forest with adjusted class weights
model_adjusted <- randomForest(Grade ~ . - Inspection.Closed.Business, data = train_data, classwt = class_weights_adjusted)

# Make predictions on the test set
predictions_adjusted <- predict(model_adjusted, newdata = test_data)

# Evaluate the model with adjusted class weights
confusionMatrix_adjusted <- confusionMatrix(predictions_adjusted, test_data$Grade)

# Get accuracy score with adjusted class weights
accuracy_adjusted <- confusionMatrix_adjusted$overall["Accuracy"]
print(paste("Random Forests Model accuracy on test set with adjusted class weights:", round(accuracy_adjusted*100,digits = 2),"%"))


# Retrieve the confusion matrix
cm <- confusionMatrix_adjusted$table

# Initialize lists to store precision, recall, and F1 score values
precision_list <- numeric()
recall_list <- numeric()
f1_score_list <- numeric()

# Loop through classes 1 to 4 to calculate precision, recall, and F1 score
for (class_label in 1:4) {
  # Extract True Positive (TP) for the class
  tp_class <- cm[class_label, class_label]
  
  # Extract False Positive (FP) for the class
  fp_class <- sum(cm[, class_label]) - tp_class
  
  # Extract False Negative (FN) for the class
  fn_class <- sum(cm[class_label, ]) - tp_class
  
  # Calculate Precision for the class
  precision_class <- tp_class / (tp_class + fp_class)
  
  # Calculate Recall for the class
  recall_class <- tp_class / (tp_class + fn_class)
  
  # Calculate F1-score for the class
  f1_class <- 2 * (precision_class * recall_class) / (precision_class + recall_class)
  
  # Store the values in the respective lists
  precision_list <- c(precision_list, precision_class)
  recall_list <- c(recall_list, recall_class)
  f1_score_list <- c(f1_score_list, f1_class)
}

# Print the results in a tabular format
cat("For Random Forests model","\n",
    "Class Label   ", paste(1:4, collapse = "       "), "\n",
    "Precision     ", paste(round(precision_list, 3), collapse = "  "), "\n",
    "Recall        ", paste(round(recall_list, 3), collapse = "  "), "\n",
    "F1 Score      ", paste(round(f1_score_list, 3), collapse = "  "), "\n")

# Calculate the macro F1 score
macro_f1 <- mean(f1_score_list)

# Print the macro F1 score
cat("Macro F1 Score for Random Forests:", round(macro_f1, 3), "\n")


# Assign predicted values as numeric
predicted_numeric <- as.numeric(predictions_adjusted)

# Assign actual values from test set as numeric
Y_test_glm_numeric <- as.numeric(test_data$Grade)  # Assuming 'Grade' is the target variable

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predicted_numeric - Y_test_glm_numeric)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Calculate Mean Squared Error (MSE)
mse <- mean((predicted_numeric - Y_test_glm_numeric)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(predicted_numeric - Y_test_glm_numeric))
cat("Mean Absolute Error (MAE):", mae, "\n")


#ROC curve 

cmc <- confusionMatrix_adjusted$byClass
# Load the pROC package (if not already loaded)
library(pROC)

# Colors for different classes (optional)
class_colors <- c("blue", "green", "red", "purple")  # Adjust as needed

# Function to calculate ROC curve for a specific class
calculate_roc <- function(class_label, Y_test_glm_numeric, predicted_numeric) {
  # Create a modified Y_test for the current class (one-vs-rest)
  Y_test_modified <- Y_test_glm_numeric
  Y_test_modified[Y_test_modified != class_label] <- 0  # Set all other classes to 0
  
  # Calculate ROC curve using roc function
  roc_curve <- roc(Y_test_modified, predicted_numeric)
  
  # Return the ROC curve object and modified Y_test
  return(list(roc_curve = roc_curve, Y_test_modified = Y_test_modified))
}


# Initialize an empty list to store modified Y_test (optional)
Y_test_modified_list <- list()

# Loop through classes and plot ROC curves
roc_curves <- list()  # Store ROC curves for all classes
for (class_label in 1:4) {
  # Calculate ROC curve and modified Y_test for the current class
  result <- calculate_roc(class_label, Y_test_glm_numeric, predicted_numeric)
  roc_curves[[class_label]] <- result$roc_curve  # Store ROC curve object
  Y_test_modified_list[[class_label]] <- result$Y_test_modified  # Store modified Y_test (optional)
}

# Create a plot to hold all ROC curves
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)")

# Add reference line (optional)
abline(a = 0, b = 1, lty = 2, col = "black")

# Loop through ROC curves and plot them on the same plot
for (class_label in 1:4) {
  roc_curve <- roc_curves[[class_label]]
  plot(roc_curve, add = TRUE, type = "l", col = class_colors[class_label], lwd = 2)
}

# Add legend (optional)
legend("topright", legend = paste("Class", 1:4), col = class_colors, lty = 1, cex = 0.8)

# Add title (optional)
title("ROC Curves (One-vs-Rest)")









# MODEL - Neural Networks with class weights and error calculations

# Load the nnet package
library(nnet)

# Train the model using Neural Networks
nn_model <- nnet(Grade ~ . - Inspection.Closed.Business, data = train_data, size = 3, maxit = 50)


# Make predictions on the test set
nn_predictions <- predict(nn_model, newdata = test_data, type = "class")

# Convert nn_predictions to factor with the same levels as test_data$Grade
nn_predictions <- factor(nn_predictions, levels = levels(test_data$Grade))

# Evaluate the model
nn_accuracy <- confusionMatrix(nn_predictions, test_data$Grade)$overall["Accuracy"]
print(paste("Neural Network model accuracy on test set:", round(nn_accuracy*100,2),"%"))

# Calculate confusion matrix
nn_confusion_matrix <- confusionMatrix(nn_predictions, test_data$Grade)

# Access the confusion matrix table
nn_confusion_matrix_table <- nn_confusion_matrix$table

# Get predicted values as numeric (assuming neural network outputs probabilities)
nn_probs <- predict(nn_model, newdata = test_data, type = "raw")  # Change "raw" if needed

# Assuming the first column of nn_probs contains the predicted probabilities for class 1
nn_predicted_numeric <- ifelse(nn_probs[, 1] > 0.5, 1, 0)  # Thresholding for class prediction (adjust as needed)

# Assign actual values from test set as numeric
Y_test_glm_numeric <- as.numeric(test_data$Grade)  # Assuming 'Grade' is the target variable

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((nn_predicted_numeric - Y_test_glm_numeric)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Calculate Mean Squared Error (MSE)
mse <- mean((nn_predicted_numeric - Y_test_glm_numeric)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(nn_predicted_numeric - Y_test_glm_numeric))
cat("Mean Absolute Error (MAE):", mae, "\n")






#MODEL - Naive Bayes
# Load the required library
library(e1071)

# Train the model using Naive Bayes
nb_model <- naiveBayes(Grade ~ . - Inspection.Closed.Business, data = train_data)

# Make predictions on the test set
nb_predictions <- predict(nb_model, newdata = test_data)

# Evaluate the model
nb_accuracy <- confusionMatrix(nb_predictions, test_data$Grade)$overall['Accuracy']
print(paste("Naive Bayes model accuracy on test set: ", round(nb_accuracy*100,2),"%"))





#MODEL-SVM
# Install required package if not already installed
install.packages("e1071")

# Load the e1071 package
library(e1071)

# Train the model using SVM
svm_model <- svm(Grade ~ . - Inspection.Closed.Business, data = train_data, kernel = "radial")

# Make predictions on the test set
svm_predictions <- predict(svm_model, newdata = test_data)

# Evaluate the model
svm_accuracy <- confusionMatrix(svm_predictions, test_data$Grade)$overall["Accuracy"]
print(paste("SVM model accuracy on test set:", round(svm_accuracy*100,2),"%"))

























install.packages("ggplot2")
library(ggplot2)

#PLOTTING VALUES
# Standardize the city names
data2$City <- toupper(data2$City)

# Get the top 10 cities with highest frequency
top_cities <- head(names(sort(table(data2$City), decreasing = TRUE)), 10)

# Subset the data for the top 10 cities
top_cities_data <- data2[data2$City %in% top_cities, ]


#PLOT -1
# Plot the frequency of the top 10 cities (red color only)
ggplot(top_cities_data, aes(x = City)) +
  geom_bar(fill = "red", color = "black") +  # Keep fill as red, color for bar outline
  labs(title = "Top 10 Cities with Highest Frequency",
       x = "City",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1, accuracy = 1, big.mark = ","))

# Create a copy of the data to avoid modifying the original data
data2_copy <- data2

# Map certain values to "No Seating"
no_seating_values <- c("Bread&Breakfast", "Limited Food Services", "Non-Profit Institution",
                       "Grocery Store", "Mobile Food Unit", "Meat/Sea Food", "Bakery", "Caterer", "School Lunch Program")

data2_copy$Type[data2_copy$Type %in% no_seating_values] <- "No Seating"

# Create a table of seating types and grades counts
heatmap_data <- table(data2_copy$Type, data2_copy$Grade)



#PLOT - 2 
# Create a heatmap
ggplot(as.data.frame(heatmap_data), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Grades by Seating Type",
       x = "Seating Type",
       y = "Grade") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a data frame with the counts of grades by seating type

plot_data <- data2 %>%
  group_by(Type, Grade) %>%
  summarise(count = n()) %>%
  mutate(Type = ifelse(Type %in% c("Bread&Breakfast", "Limited Food Services", "Non-Profit Institution", 
                                   "Grocery Store", "Mobile Food Unit", "Meat/Sea Food", "Bakery", "Caterer",
                                   "School Lunch Program"), "No Seating", Type))



#PLOT - 3
# Create a grouped bar plot
ggplot(plot_data, aes(x = Type, y = count, fill = Grade)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Grades by Seating Type",
       x = "Seating Type",
       y = "Count",
       fill = "Grade") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Install required libraries (if not already installed)
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("scales")

# Load the libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Convert 'Inspection.Date' to date format (optional, for future reference)
# data2$Inspection.Date <- as.Date(data2$`Inspection.Date`, format = "%m/%d/%Y")

# Calculate average score every month
monthly_scores <- data2 %>%
  group_by(year = year(Inspection.Date), month = month(Inspection.Date)) %>%
  summarise(avg_score = mean(Inspection.Score))

# Check for missing dates (optional)
if (sum(is.na(monthly_scores$Inspection.Date)) > 0) {
  warning("Missing dates found in Inspection.Date. Consider handling them before plotting.")
  # You can choose to remove rows with missing dates here (e.g., monthly_scores <- na.omit(monthly_scores))
}

# Optional: Subset data for a smaller date range (if needed)
# For example, to focus on years 2020-2023:
# subset_years <- 2020:2023
# monthly_scores <- monthly_scores %>% filter(year %in% subset_years)

# Combine year and month into a date object
monthly_scores$Inspection.Date <- as.Date(paste(monthly_scores$year, monthly_scores$month, sep = "-"), format = "%Y-%m")

# Plot average score by month (using Inspection.Date on x-axis)
ggplot(monthly_scores, aes(x = Inspection.Date, y = avg_score)) +
  geom_line(color = "blue") +
  labs(title = "Average Inspection Score by Month",
       x = "Month",  # Adjusted x-axis label
       y = "Average Score") +
  scale_x_date(labels = date_format("%b"))  # Now works with Inspection.Date

# Additional customizations (optional)
# You can further customize the plot by:
# - Changing line style: geom_line(color = "blue", linetype = "dashed")
# - Adding gridlines: theme(panel.grid.major = TRUE)
# - Adding reference lines: geom_hline(yintercept = mean(monthly_scores$avg_score), color = "red")






#PLOT - 
#Box Plot of Scores by Risk Category:
ggplot(data2, aes(x = Risk_Category, y = Inspection.Score)) +
  geom_boxplot() +
  labs(title = "Distribution of Inspection Scores by Risk Category",
       x = "Risk Category",
       y = "Inspection Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
























