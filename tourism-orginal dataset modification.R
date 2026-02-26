install.packages("tidytext")
install.packages("dplyr")
install.packages("tidyr")
install.packages("textdata")
install.packages("ggplot2")

library(tidytext)
library(dplyr)
library(tidyr)
library(textdata)
library(ggplot2)
# Load tourism & OYO hotel reviews
tourism_reviews <- read.csv("C:\\Users\\adona\\OneDrive\\Desktop\\masters project\\tourism_orginal.csv")

tourism_reviews <- tourism_reviews %>%
  rename(
    review_text = Review
  )


# Keep only necessary columns (City, Review Text)
tourism_reviews <- tourism_reviews %>% select(City, review_text, Rating)
# Load Bing sentiment lexicon
bing_lexicon <- get_sentiments("bing")  

# Tokenize text (convert each review into individual words)
tourism_sentiment <- tourism_reviews %>%
  unnest_tokens(word, review_text) %>%  # Split text into words
  inner_join(bing_lexicon, by = "word", relationship = "many-to-many") %>%  # Fix many-to-many issue
  count(City, sentiment) %>%  # Count positive and negative words per city
  spread(sentiment, n, fill = 0) %>%  # Convert data to wide format
  mutate(Sentiment_Score = (positive - negative) / (positive + negative + 1))  # Compute



tourism_sentiment <- tourism_sentiment %>%
  mutate(Sentiment_Label = case_when(
    Sentiment_Score > 0.1  ~ "Positive",
    Sentiment_Score < -0.1 ~ "Negative",
    TRUE                   ~ "Neutral"
  ))

# View the sentiment results
print(head(tourism_sentiment))

# Merge Sentiment Scores back to main dataset
final_tourism_data <- left_join(tourism_reviews, tourism_sentiment, by = "City")

library(dplyr)

# Step 1: Compute average rating per city
avg_rating <- tourism_reviews %>%
  group_by(City) %>%
  summarise(Avg_Rating = mean(Rating, na.rm = TRUE))  # Compute average rating per city

# Step 2: Remove duplicate city rows from sentiment analysis (Keep one row per city)
tourism_sentiment_unique <- tourism_sentiment %>%
  distinct(City, .keep_all = TRUE) %>%
  select(City, positive, negative, Sentiment_Score, Sentiment_Label)  # Keep only relevant columns

# Step 3: Merge average rating with sentiment analysis results
final_tourism_data <- left_join(avg_rating, tourism_sentiment_unique, by = "City")

# Step 4: View the cleaned dataset
print(head(final_tourism_data))


# View the final dataset with sentiment analysis
print(head(final_tourism_data))

ggplot(tourism_sentiment, aes(x = Sentiment_Label, fill = Sentiment_Label)) +
  geom_bar() +
  labs(title = "Sentiment Distribution of Tourism Reviews", x = "Sentiment", y = "Count") +
  theme_minimal()
# Save the final tourism data to a CSV file
write.csv(final_tourism_data, "C:\\Users\\adona\\OneDrive\\Desktop\\masters project\\final_tourism_data.csv", row.names = FALSE)

# Check if the file is saved successfully
print("CSV file saved successfully!")


