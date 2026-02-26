# Load necessary libraries
library(readr)
library(caret)
library(ggplot2)
library(reshape2)

# Load the CSV file
df <- read_csv("C:/Users/adona/Downloads/training_data_combined_reviews_result.csv")

# Convert 'Class' to factor (true labels)
df$true_label <- as.factor(df$Class)

# Map turing_test_pass to predicted classes (modify logic as needed)
df$predicted_label <- ifelse(df$turing_test_pass == TRUE, "Street Scams & Fraud", "good review")
df$predicted_label <- as.factor(df$predicted_label)

# Ensure factor levels match
df$predicted_label <- factor(df$predicted_label, levels = levels(df$true_label))

# Create confusion matrix
cm <- table(Predicted = df$predicted_label, Actual = df$true_label)

# Convert to data frame for plotting
cm_df <- as.data.frame(cm)

# Plot heatmap
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1.2, size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix Heatmap", x = "Actual Class", y = "Predicted Class", fill = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
