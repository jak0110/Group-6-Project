# Claude link:https://claude.ai/share/7babf177-f185-4c1a-8462-6f4611f8d2ee

#Import data
spotify_data <- read.csv("C:/Users/jaken/OneDrive/Desktop/dataset.csv")
spotify_data <- spotify_data[-1]
#Remove missing values
spotify_data <- na.omit(spotify_data)
#List of all numerical data
numlist <- c("popularity",
             "duration_ms",
             "danceability",
             "energy",
             "loudness",
             "speechiness",
             "acousticness",
             "instrumentalness",
             "liveness",
             "valence",
             "tempo"
)
#List of categorical data
catdata <- c("explicit",
             "mode",
             "track_genre",
             "artists"
)
# Train/test split (80/20)
set.seed(123)
train_index <- sample(seq_len(nrow(spotify_data)), size = 0.8 * nrow(spotify_data))
train_data  <- spotify_data[train_index, ]
test_data   <- spotify_data[-train_index, ]

cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows: ", nrow(test_data),  "\n")

install.packages("randomForest")
library(randomForest)

# Prepare data with relevant variables
rf_vars <- c("popularity", "duration_ms", "danceability", "energy",
             "loudness", "speechiness", "acousticness", "instrumentalness",
             "liveness", "valence", "tempo", "explicit", "mode", "track_genre")

rf_data <- spotify_data[, rf_vars]

# Use same train/test split indices as before
rf_train <- rf_data[train_index, ]
rf_test  <- rf_data[-train_index, ]

# Encode explicit and mode as numeric
rf_train$explicit <- as.numeric(as.factor(rf_train$explicit))
rf_test$explicit  <- as.numeric(as.factor(rf_test$explicit))
rf_train$mode     <- as.numeric(as.factor(rf_train$mode))
rf_test$mode      <- as.numeric(as.factor(rf_test$mode))

# Encode track_genre as mean popularity from training data only
genre_means          <- tapply(rf_train$popularity, rf_train$track_genre, mean)
rf_train$track_genre <- as.numeric(genre_means[as.character(rf_train$track_genre)])
rf_test$track_genre  <- as.numeric(genre_means[as.character(rf_test$track_genre)])

# Train random forest
# Use a sample of training data if dataset is very large
set.seed(123)
sample_index <- sample(seq_len(nrow(rf_train)), size = min(10000, nrow(rf_train)))
rf_train_sample <- rf_train[sample_index, ]

# Faster random forest settings
set.seed(123)
rf_model <- randomForest(
  popularity ~ .,
  data      = rf_train_sample,
  ntree     = 100,   # reduced from 500
  mtry      = 5,     # increased so each tree is more informative
  maxnodes  = 50,    # limits tree depth
  importance = TRUE
)

print(rf_model)

# Evaluate on test data
rf_preds     <- predict(rf_model, newdata = rf_test)
actuals      <- rf_test$popularity

rf_rmse      <- sqrt(mean((actuals - rf_preds)^2))
rf_mae       <- mean(abs(actuals - rf_preds))
ss_res       <- sum((actuals - rf_preds)^2)
ss_tot       <- sum((actuals - mean(actuals))^2)
rf_r_squared <- 1 - (ss_res / ss_tot)

cat("Random Forest Test Results:\n")
cat("RMSE:      ", round(rf_rmse,      4), "\n")
cat("MAE:       ", round(rf_mae,       4), "\n")
cat("R-squared: ", round(rf_r_squared, 4), "\n")

# Variable importance plot
varImpPlot(rf_model,
           main = "Variable Importance - Random Forest")

