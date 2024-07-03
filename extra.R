




# ... Your code up to calculating confusion matrix (cm)

# Function to calculate TPR, FPR for a specific class (assuming cm is available)
calculate_tpr_fpr <- function(class_label) {
  # Extract True Positive (TP) for the class
  tp_class <- cm[class_label, class_label]
  
  # Extract False Positive (FP) for the class
  fp_class <- sum(cm[, class_label]) - tp_class
  
  # Extract False Negative (FN) for the class
  fn_class <- sum(cm[class_label, ]) - tp_class
  
  # Calculate TPR and FPR (handle potential division by zero)
  tpr <- ifelse(tp_class + fn_class == 0, 0, tp_class / (tp_class + fn_class))
  fpr <- ifelse(fp_class + sum(cm[, -class_label]) == 0, 0, fp_class / (sum(cm[, -class_label]) + fp_class))
  
  return(list(tpr = tpr, fpr = fpr))
}

# Calculate TPR and FPR for class 1 only
class_label <- 1  # Specify class 1
results <- calculate_tpr_fpr(class_label)

# Extract TPR and FPR for class 1
tpr_class1 <- results$tpr
fpr_class1 <- results$fpr

# Colors for class 1 (optional, adjust)
class_color <- "blue"

# Plot ROC curve for class 1
plot(fpr_class1, tpr_class1, main = "ROC Curve (Class 1)", 
     col = class_color, lwd = 2, pch = 1, cex = 2)  # Add class label as point character

# Add reference line (optional)
abline(a = 0, b = 1, lty ="dashed", col = "black")

# Axis labels
xlabel("False Positive Rate (FPR)")
ylabel("True Positive Rate (TPR)")

# Add legend (optional)
legend("topright", legend = "Class 1", col = class_color, lty = 1, cex = 0.8)




# Calculate TPR and FPR for class 2
class_label <- 2  # Specify class 2
results <- calculate_tpr_fpr(class_label)

# Extract TPR and FPR for class 2
tpr_class2 <- results$tpr
fpr_class2 <- results$fpr

# Colors for class 2 (optional, adjust)
class_color <- "green"  # Choose a different color

# Plot ROC curve for class 2
plot(fpr_class2, tpr_class2, main = "ROC Curve (Class 2)", 
     col = class_color, lwd = 2, pch = 2, cex = 2)  # Add class label as point character

# Add reference line (optional)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Axis labels
xlabel("False Positive Rate (FPR)")
ylabel("True Positive Rate (TPR)")

# Add legend (optional)
legend("topright", legend = "Class 2", col = class_color, lty = 1, cex = 0.8)


# Calculate TPR and FPR for class 3
class_label <- 3  # Specify class 3
results <- calculate_tpr_fpr(class_label)

# Extract TPR and FPR for class 3
tpr_class3 <- results$tpr
fpr_class3 <- results$fpr

# Colors for class 3 (optional, adjust)
class_color <- "red"  # Choose a different color

# Plot ROC curve for class 3
plot(fpr_class3, tpr_class3, main = "ROC Curve (Class 3)", 
     col = class_color, lwd = 2, pch = 3, cex = 2)  # Add class label as point character

# Add reference line (optional)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Axis labels
xlabel("False Positive Rate (FPR)")
ylabel("True Positive Rate (TPR)")

# Add legend (optional)
legend("topright", legend = "Class 3", col = class_color, lty = 1, cex = 0.8)



# Calculate TPR and FPR for class 4
class_label <- 4  # Specify class 4
results <- calculate_tpr_fpr(class_label)

# Extract TPR and FPR for class 4
tpr_class4 <- results$tpr
fpr_class4 <- results$fpr

# Colors for class 4 (optional, adjust)
class_color <- "purple"  # Choose a different color

# Plot ROC curve for class 4
plot(fpr_class4, tpr_class4, main = "ROC Curve (Class 4)", 
     col = class_color, lwd = 2, pch = 4, cex = 2)  # Add class label as point character

# Add reference line (optional)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Axis labels
xlabel("False Positive Rate (FPR)")
ylabel("True Positive Rate (TPR)")

# Add legend (optional)
legend("topright", legend = "Class 4", col = class_color, lty = 1, cex = 0.8)





fpr_values <- c(0.181457, 0.2695858, 0.05947497, 0.005052413)

# TPR (True Positive Rate) values for all classes
tpr_values <- c(0.639677, 0.5597546, 0.3884758, 0.1410256)
# Colors for different classes (optional, adjust)
class_colors <- c("blue", "green", "red", "purple")  # Adjust based on the number of classes

# Loop through classes and plot ROC curves
for (i in 1:length(fpr_values)) {
  fpr <- fpr_values[i]
  tpr <- tpr_values[i]
  class_label <- i  # Optional: Add class label to the plot (adjust as needed)
  
  # Plot ROC curve for current class
  plot(fpr, tpr, type = "l", col = class_colors[i], lwd = 2, 
       main = "ROC Curve", xlab = "False Positive Rate (FPR)", 
       ylab = "True Positive Rate (TPR)")
  
  # Add reference line (optional)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # Add class label to the plot (optional)
  if (!is.null(class_label)) {
    text(fpr[length(fpr)], tpr[length(tpr)], paste("Class", class_label), col = class_colors[i])
  }
}

# Add legend (optional)
legend("topright", legend = paste("Class", 1:length(fpr_values)), col = class_colors, lty = 1, cex = 0.8)


