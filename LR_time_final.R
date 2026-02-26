# Load required packages
library(tidyverse)
library(forecast)
library(caret)
library(ggplot2)
library(gridExtra)

# Set file paths
input_path <- "C:/Users/adona/Downloads/classified_cities_kmeans_with_year.csv"
output_path <- "C:/Users/adona/Downloads/crime_data_with_2025_predictions_R.csv"

# Function to predict next year's value with accuracy metrics
predict_next_year <- function(city_data, variable) {
  if (nrow(city_data) < 2) {
    return(list(prediction = city_data[[variable]][nrow(city_data)], 
                rmse = NA, 
                r_squared = NA))
  }
  
  # Prepare data
  X <- city_data$Year
  y <- city_data[[variable]]
  
  # Train linear model
  model <- lm(y ~ X, data = city_data)
  
  # Make prediction
  next_year <- data.frame(X = max(X) + 1)
  prediction <- predict(model, newdata = next_year)
  
  # Calculate accuracy metrics
  predictions <- predict(model)
  rmse <- sqrt(mean((y - predictions)^2))
  r_squared <- summary(model)$r.squared
  
  # Validate predictions
  if (variable %in% c('High-Risk', 'Low-Risk', 'Medium-Risk', 'Total_Crimes')) {
    prediction <- max(0, round(prediction))
  } else if (variable == 'High_Risk_Percentage') {
    prediction <- max(0, min(1, prediction))
  }
  
  return(list(prediction = prediction, 
              rmse = rmse, 
              r_squared = r_squared))
}

# Function to create city-specific visualizations
create_visualizations <- function(city_data, city_name) {
  # Time series plot for each crime type
  p1 <- ggplot(city_data, aes(x = Year, y = `High-Risk`)) +
    geom_line(color = "red") +
    geom_point() +
    labs(title = paste("High-Risk Crimes in", city_name),
         y = "Count") +
    theme_minimal()
  
  p2 <- ggplot(city_data, aes(x = Year, y = `Medium-Risk`)) +
    geom_line(color = "orange") +
    geom_point() +
    labs(title = paste("Medium-Risk Crimes in", city_name),
         y = "Count") +
    theme_minimal()
  
  p3 <- ggplot(city_data, aes(x = Year, y = `Low-Risk`)) +
    geom_line(color = "green") +
    geom_point() +
    labs(title = paste("Low-Risk Crimes in", city_name),
         y = "Count") +
    theme_minimal()
  
  p4 <- ggplot(city_data, aes(x = Year, y = Total_Crimes)) +
    geom_line(color = "blue") +
    geom_point() +
    labs(title = paste("Total Crimes in", city_name),
         y = "Count") +
    theme_minimal()
  
  # Combine plots
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

# New function for combined trend visualization
create_combined_trend_plot <- function(data) {
  # Prepare data for plotting
  trend_data <- data %>%
    filter(Year >= 2020) %>%
    select(City, Year, `High-Risk`, `Medium-Risk`, `Low-Risk`, Total_Crimes) %>%
    pivot_longer(
      cols = -c(City, Year),
      names_to = "Crime_Type",
      values_to = "Count"
    ) %>%
    mutate(
      Crime_Type = factor(Crime_Type, 
                          levels = c("High-Risk", "Medium-Risk", "Low-Risk", "Total_Crimes"),
                          labels = c("High Risk", "Medium Risk", "Low Risk", "Total Crimes"))
    )
  
  # Calculate city averages
  avg_trends <- trend_data %>%
    group_by(Year, Crime_Type) %>%
    summarise(Avg_Count = mean(Count, na.rm = TRUE), .groups = "drop")
  
  # Create the plot
  ggplot(avg_trends, aes(x = Year, y = Avg_Count, color = Crime_Type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(
      values = c("High Risk" = "red3", 
                 "Medium Risk" = "darkorange", 
                 "Low Risk" = "green4", 
                 "Total Crimes" = "blue3")
    ) +
    labs(
      title = "Average Crime Trends Across All Cities (2020-2025)",
      subtitle = "Dashed line marks start of predictions",
      y = "Average Crime Count",
      color = "Crime Type"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = seq(2020, 2025, 1)) +
    geom_vline(xintercept = 2023.5, linetype = "dashed", color = "gray50") +
    annotate("text", x = 2024.25, y = max(avg_trends$Avg_Count)*0.9, 
             label = "Prediction Period", color = "gray40", size = 5)
}

# Main function
main <- function() {
  # Load data
  df <- read_csv(input_path, show_col_types = FALSE)
  cat("Successfully loaded data from", input_path, "\n")
  
  # Create predictions
  predictions <- list()
  accuracy_metrics <- list()
  variables <- c('High-Risk', 'Low-Risk', 'Medium-Risk', 'Total_Crimes', 'High_Risk_Percentage')
  
  for (city in unique(df$City)) {
    city_data <- df %>% filter(City == city) %>% arrange(Year)
    
    # Create visualizations for each city
    create_visualizations(city_data, city)
    
    city_pred <- data.frame(City = city, Year = 2025)
    city_accuracy <- data.frame(City = city)
    
    for (var in variables) {
      result <- predict_next_year(city_data, var)
      city_pred[[var]] <- result$prediction
      
      # Store accuracy metrics
      city_accuracy[[paste0(var, "_RMSE")]] <- result$rmse
      city_accuracy[[paste0(var, "_R2")]] <- result$r_squared
    }
    
    # Get most recent cluster
    city_pred$KMeans_Cluster <- city_data$KMeans_Cluster[nrow(city_data)]
    
    predictions <- bind_rows(predictions, city_pred)
    accuracy_metrics <- bind_rows(accuracy_metrics, city_accuracy)
  }
  
  # Combine and save results
  complete_df <- bind_rows(df, predictions)
  write_csv(complete_df, output_path)
  cat("Predictions saved to", output_path, "\n")
  
  # Save accuracy metrics
  accuracy_path <- "C:/Users/adona/Downloads/crime_prediction_accuracy_metrics_R.csv"
  write_csv(accuracy_metrics, accuracy_path)
  cat("Accuracy metrics saved to", accuracy_path, "\n")
  
  # Create and save combined trend plot
  combined_plot <- create_combined_trend_plot(complete_df)
  ggsave("C:/Users/adona/Downloads/combined_crime_trends.png", 
         combined_plot, width = 12, height = 8, dpi = 300)
  cat("Combined trend plot saved\n")
  print(combined_plot)
  # Print summary
  cat("\n2025 Predictions Preview:\n")
  print(head(predictions))
  
  cat("\nAccuracy Metrics Summary:\n")
  print(summary(accuracy_metrics %>% select(ends_with("_RMSE"), ends_with("_R2"))))
}

# Execute main function
main()

# ===============================
# Summarize RMSE, MAE, MAPE, R² by Risk Level (across all cities)
# ===============================
library(dplyr)
library(readr)

# Load the accuracy metrics CSV
accuracy_path <- "C:/Users/adona/Downloads/crime_prediction_accuracy_metrics_R.csv"
accuracy_metrics <- read_csv(accuracy_path, show_col_types = FALSE)

# Calculate mean metrics for each risk level
risk_eval_summary <- tibble(
  Risk_Level = c("High-Risk", "Medium-Risk", "Low-Risk"),
  
  RMSE = c(
    mean(accuracy_metrics$`High-Risk_RMSE`, na.rm = TRUE),
    mean(accuracy_metrics$`Medium-Risk_RMSE`, na.rm = TRUE),
    mean(accuracy_metrics$`Low-Risk_RMSE`, na.rm = TRUE)
  ),
  
  MAE = c(
    mean(accuracy_metrics$`High-Risk_MAE`, na.rm = TRUE),
    mean(accuracy_metrics$`Medium-Risk_MAE`, na.rm = TRUE),
    mean(accuracy_metrics$`Low-Risk_MAE`, na.rm = TRUE)
  ),
  
  MAPE = c(
    mean(accuracy_metrics$`High-Risk_MAPE`, na.rm = TRUE),
    mean(accuracy_metrics$`Medium-Risk_MAPE`, na.rm = TRUE),
    mean(accuracy_metrics$`Low-Risk_MAPE`, na.rm = TRUE)
  ),
  
  R2 = c(
    mean(accuracy_metrics$`High-Risk_R2`, na.rm = TRUE),
    mean(accuracy_metrics$`Medium-Risk_R2`, na.rm = TRUE),
    mean(accuracy_metrics$`Low-Risk_R2`, na.rm = TRUE)
  )
)

# Print the summarized table
cat("\n📊 Linear Regression Evaluation by Risk Level:\n")
print(risk_eval_summary)
