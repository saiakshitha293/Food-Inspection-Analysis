data1<-read.csv(file.choose(),header=T)
View(data1)
data1 <- subset(data1, select = -c(RA_Report..,PRI_FDA.Industry.Code,CI_Age.Unit))
View(data1)
names(data1) <- c("Created_Date","Start_Date","Product_Role","Product_Name","Product_Industry","Age","Gender","Outcomes","Symptoms")
View(data1)


install.packages("psych")
library(psych)



#COLUMN - AGE
#removes all age null value rows --->80k-31k
data1 <- data1[complete.cases(data1$Age), ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#remove the values ==0
data1 <- data1[data1$Age != 0, ]

# Remove observations with Age values above 100 and equal to zero
data1 <- data1[data1$Age <=100, ]



#COLUMN - GENDER
# Replace "Not Available", "Not Reported", and "Unknown" with "Unknown"
data1$Gender[data1$Gender %in% c("Not Available", "Not Reported", "Unknown")] <- "Unknown"



#COLUMN - SYMPTOMS
# Install required packages if not already installed
install.packages(c("tm", "wordcloud", "RColorBrewer"))

# Load required libraries
library(tm)
library(wordcloud)
library(RColorBrewer)

# Tokenize symptoms column
data1$Tokens <- sapply(data1$Symptoms, function(x) unlist(strsplit(x, ",\\s*")))

# Combine all tokens into a single vector
all_tokens <- unlist(data1$Tokens)

# Remove quotes and other unwanted characters
all_tokens_clean <- gsub("[[:punct:]]", "", all_tokens)
all_tokens_clean <- gsub("'", "", all_tokens_clean)
all_tokens_clean <- gsub('"', "", all_tokens_clean)

# Create a table of word frequencies
word_freq <- table(all_tokens_clean)

# Choose the top 100 most frequent words
top_words <- head(sort(word_freq, decreasing = TRUE), 100)

# Generate the word cloud
wordcloud(words = names(top_words), freq = top_words,
          scale = c(5, 0.5), min.freq = 1, max.words = Inf,
          random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

#500
# Choose the top 500 most frequent words
top_words2 <- head(sort(word_freq, decreasing = TRUE), 500)

# Generate the word cloud
wordcloud(words = names(top_words), freq = top_words,
          scale = c(5, 0.5), min.freq = 1, max.words = Inf,
          random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))




#COLUMN - Product_Industry----->lite
# Tokenize Product_Industry column
data1$Product_Industry_Tokens <- sapply(data1$Product_Industry, function(x) unlist(strsplit(x, "\\s+")))

# Combine all tokens into a single vector
all_tokens_pi <- unlist(data1$Product_Industry_Tokens)

# Remove quotes and other unwanted characters
all_tokens_clean_pi <- gsub("[[:punct:]]", "", all_tokens_pi)
all_tokens_clean_pi <- gsub("'", "", all_tokens_clean_pi)
all_tokens_clean_pi <- gsub('"', "", all_tokens_clean_pi)

# Create a table of word frequencies
word_freq_pi <- table(all_tokens_clean_pi)

# Choose the top 100 most frequent words
top_words_pi <- head(sort(word_freq_pi, decreasing = TRUE), 100)

# Generate the word cloud
wordcloud(words = names(top_words_pi), freq = top_words_pi,
          scale = c(5, 0.5), min.freq = 1, max.words = Inf,
          random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# Choose the top 500 most frequent words
top_words2_pi <- head(sort(word_freq_pi, decreasing = TRUE), 500)

# Generate the word cloud
wordcloud(words = names(top_words2_pi), freq = top_words2_pi,
          scale = c(5, 0.5), min.freq = 1, max.words = Inf,
          random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))











#PLOTTING OF GRAPHS:
# Violin plot for Age
ggplot(data1, aes(x = "", y = Age, fill = Gender)) +
  geom_violin() +
  labs(title = "Violin Plot of Age")

# Box plot for Gender
ggplot(data1, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Box Plot of Gender")

# Scatter plot for Age vs. Gender----->Not nice
ggplot(data1, aes(x = Age, y = Gender, color = Gender)) +
  geom_point() +
  labs(title = "Scatter Plot of Age by Gender")

# Scatter plot for Age vs. Gender
ggplot(data1, aes(x = Gender, y = Age, color = Gender)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), size = 2.5) +
  labs(title = "Scatter Plot of Age by Gender", x = "Gender", y = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

# Density plot for Age
ggplot(data1, aes(x = Age, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age")

# Double Density Plot of Age by Gender----->not good
ggplot(data1, aes(x = Age, fill = Gender)) +
  geom_density(alpha = 0.5, adjust = 2) +
  facet_wrap(~ Gender, scales = "free") +
  labs(title = "Double Density Plot of Age by Gender", x = "Age", y = "Density") +
  theme_minimal()

# Subset data for only Female and Male
female_data <- data1[data1$Gender == "Female", ]
male_data <- data1[data1$Gender == "Male", ]

# Double Density Plot of Age by Gender
ggplot() +
  geom_density(data = female_data, aes(x = Age, y = ..density.., fill = "Female"), alpha = 0.5) +
  geom_density(data = male_data, aes(x = Age, y = ..density.., fill = "Male"), alpha = 0.5) +
  labs(title = "Double Density Plot of Age by Gender", x = "Age", y = "Density", fill = "Gender") +
  scale_fill_manual(values = c("Female" = "blue", "Male" = "red")) +
  theme_minimal()

# Box Plot of Age by Gender
ggplot(data1, aes(x = Gender, y = Age, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age by Gender", x = "Gender", y = "Age") +
  theme_minimal()

# Kernel Density Plot of Age by Gender
ggplot(data1, aes(x = Age, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Kernel Density Plot of Age by Gender", x = "Age", y = "Density", fill = "Gender") +
  theme_minimal()

























# Assuming 'data1' is your dataset
# Create age groups
data1$Age_Group <- cut(data1$Age, breaks = seq(0, 100, by = 10), labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-100"), include.lowest = TRUE)

# Plot a bar graph
# Install the ggplot2 package
install.packages("ggplot2")

# Load the ggplot2 package
library(ggplot2)
ggplot(data1, aes(x = Age_Group)) +
  geom_bar() +
  labs(title = "Frequency of Adverse Effects by Age Group", x = "Age Group", y = "Frequency")

# Plot a histogram of ages
ggplot(data1, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Ages", x = "Age", y = "Frequency")

# Plot a histogram of ages with a smaller binwidth
ggplot(data1, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Ages", x = "Age", y = "Frequency")

# Plot a bar plot of frequencies of unique values in the Gender column
ggplot(data1, aes(x = Gender)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Frequency of Unique Values", x = "Gender", y = "Frequency")







#dont consider yet


# Load required libraries
library(tm)
library(caret)

# Tokenize symptoms column
data1$Symptoms <- as.character(data1$Symptoms) # Ensure Symptoms column is character type
corpus <- Corpus(VectorSource(data1$Symptoms))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
# Convert DTM to a matrix
symptoms_matrix <- as.matrix(dtm)

# Prepare data for modeling
data1_processed <- cbind(data1[c("Age", "Gender", "Product_Industry")], symptoms_matrix)

# Split data into training and testing sets
set.seed(123)
trainIndex <- sample(1:2, nrow(data1_processed), replace = TRUE, prob = c(0.8, 0.2))
train_data <- data1_processed[trainIndex == 1, ]
test_data <- data1_processed[trainIndex == 2, ]

# Train the model
model <- train(Product_Industry ~ ., data = train_data, method = "rf")

# Make predictions
predictions <- predict(model, newdata = test_data)

# Evaluate the model
cm <- confusionMatrix(predictions, test_data$Product_Industry)

# Print the accuracy
print(paste("Model accuracy on test set:", cm$overall["Accuracy"]))
