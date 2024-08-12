#  WORKING BUT WITH MAE=20


# Load required libraries
library(dplyr)
library(caret)

# Read the data
data <- read.csv("spotify_songs.csv")

# Select relevant columns
selected_data <- data %>%
  select(track_popularity, danceability, energy, loudness,liveness,
         acousticness, instrumentalness, duration_ms)

# Split data into training and testing sets (set seed for reproducibility)
set.seed(123)
train_index <- sample(nrow(selected_data), 0.8 * nrow(selected_data))
train_data <- selected_data[train_index, ]
test_data <- selected_data[-train_index, ]

# Feature Transformation (applied to both training and test data)
transformed_data <- train_data %>%
  mutate(duration_ms_log = log(duration_ms),
         danceability_squared = danceability^2,
         energy_danceability = energy * danceability)

test_data_transformed <- test_data %>%  
  mutate(duration_ms_log = log(duration_ms),
         danceability_squared = danceability^2, 
         energy_danceability = energy * danceability)

# Feature Selection using Recursive Feature Elimination
control <- rfeControl(functions = lmFuncs, method = "repeatedcv")
lm_rfe <- rfe(transformed_data[, -1], transformed_data$track_popularity, sizes = c(3:8), rfeControl = control)
final_features <- predictors(lm_rfe)

# Build linear regression model with selected features
model <- lm(track_popularity ~ ., data = transformed_data[, c("track_popularity", final_features)])



# Make predictions for a hypothetical song
hypothetical_song <- data.frame(
  danceability = 0.7,
  energy = 0.8,
  loudness = -5,
  acousticness = 0.2,
  instrumentalness = 0.05,
  liveness = 0.204,
  duration_ms = 180000
)

# Transform hypothetical song data
hypothetical_song_transformed <- hypothetical_song %>%
  mutate(duration_ms_log = log(duration_ms),
         danceability_squared = danceability^2,
         energy_danceability = energy * danceability)


# Predict popularity for the hypothetical song
predicted_popularity <- predict(model, newdata = hypothetical_song_transformed)


# Evaluate model on test set
test_predictions <- predict(model, newdata = test_data_transformed)
mae <- mean(abs(test_predictions - test_data$track_popularity))
cat("Mean Absolute Error on Test Set:", mae, "\n")


cat("Predicted popularity for the hypothetical song:", predicted_popularity, "\n")


