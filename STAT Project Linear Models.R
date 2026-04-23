# Claude link: https://claude.ai/share/7babf177-f185-4c1a-8462-6f4611f8d2ee

# Import data
spotify_data <- read.csv("C:/Users/jaken/OneDrive/Desktop/dataset.csv")
spotify_data <- spotify_data[-1]

# Remove missing values
spotify_data <- na.omit(spotify_data)

# List of all numerical data
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

# List of categorical data
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

# Gen1 - train only
model_stats <- data.frame(
  variable = character(),
  p_value  = numeric(),
  aic      = numeric()
)

for (num in numlist) {
  if (num != "popularity") {
    model <- lm(popularity ~ train_data[[num]], data = train_data)
    p_val <- summary(model)$coefficients[2, 4]
    aic   <- AIC(model)
    
    model_stats <- rbind(model_stats, data.frame(
      variable = num,
      p_value  = p_val,
      aic      = aic
    ))
  }
}

model_stats <- model_stats[order(model_stats$aic), ]
print(model_stats)

# Filter to top 50% AIC
median_aic    <- median(model_stats$aic)
top_models    <- model_stats[model_stats$aic <= median_aic, ]
top_variables <- top_models$variable

cat("Top 50% AIC variables:", paste(top_variables, collapse = ", "), "\n")

# Gen2 - train only
combos <- combn(top_variables, 2, simplify = FALSE)

gen2_stats <- data.frame(
  variables = character(),
  p_value1  = numeric(),
  p_value2  = numeric(),
  aic       = numeric()
)

for (combo in combos) {
  var1 <- combo[1]
  var2 <- combo[2]
  
  formula    <- as.formula(paste("popularity ~", var1, "+", var2))
  model      <- lm(formula, data = train_data)
  coef_table <- summary(model)$coefficients
  
  p_val1 <- ifelse(var1 %in% rownames(coef_table), coef_table[var1, 4], NA)
  p_val2 <- ifelse(var2 %in% rownames(coef_table), coef_table[var2, 4], NA)
  aic    <- AIC(model)
  
  gen2_stats <- rbind(gen2_stats, data.frame(
    variables = paste(var1, "+", var2),
    p_value1  = p_val1,
    p_value2  = p_val2,
    aic       = aic
  ))
}

gen2_stats <- gen2_stats[order(gen2_stats$aic), ]
print(gen2_stats)

# Filter gen2 to top 50% AIC
median_aic_gen2 <- median(gen2_stats$aic)
top_gen2        <- gen2_stats[gen2_stats$aic <= median_aic_gen2, ]
top_gen2_vars   <- unique(trimws(unlist(strsplit(top_gen2$variables, "\\+"))))

cat("Top 50% gen2 variables:", paste(top_gen2_vars, collapse = ", "), "\n")

# Gen3 - train only
combos_gen3 <- combn(top_gen2_vars, 3, simplify = FALSE)

gen3_stats <- data.frame(
  variables = character(),
  p_value1  = numeric(),
  p_value2  = numeric(),
  p_value3  = numeric(),
  aic       = numeric()
)

for (combo in combos_gen3) {
  var1 <- combo[1]
  var2 <- combo[2]
  var3 <- combo[3]
  
  formula    <- as.formula(paste("popularity ~", var1, "+", var2, "+", var3))
  model      <- lm(formula, data = train_data)
  coef_table <- summary(model)$coefficients
  
  p_val1 <- ifelse(var1 %in% rownames(coef_table), coef_table[var1, 4], NA)
  p_val2 <- ifelse(var2 %in% rownames(coef_table), coef_table[var2, 4], NA)
  p_val3 <- ifelse(var3 %in% rownames(coef_table), coef_table[var3, 4], NA)
  aic    <- AIC(model)
  
  gen3_stats <- rbind(gen3_stats, data.frame(
    variables = paste(var1, "+", var2, "+", var3),
    p_value1  = p_val1,
    p_value2  = p_val2,
    p_value3  = p_val3,
    aic       = aic
  ))
}

gen3_stats <- gen3_stats[order(gen3_stats$aic), ]
print(gen3_stats)

# Final evaluation - top 5 gen3 models on test data only
top5_gen3 <- head(gen3_stats, 5)

top5_results <- data.frame(
  variables = character(),
  aic       = numeric(),
  rmse      = numeric(),
  r_squared = numeric()
)

for (i in seq_len(nrow(top5_gen3))) {
  vars <- trimws(unlist(strsplit(as.character(top5_gen3$variables[i]), "\\+")))
  var1 <- vars[1]
  var2 <- vars[2]
  var3 <- vars[3]
  
  formula <- as.formula(paste("popularity ~", var1, "+", var2, "+", var3))
  model   <- lm(formula, data = train_data)
  
  preds   <- predict(model, newdata = test_data)
  actuals <- test_data$popularity
  
  aic       <- AIC(model)
  rmse      <- sqrt(mean((actuals - preds)^2))
  ss_res    <- sum((actuals - preds)^2)
  ss_tot    <- sum((actuals - mean(actuals))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  top5_results <- rbind(top5_results, data.frame(
    variables = top5_gen3$variables[i],
    aic       = round(aic,       2),
    rmse      = round(rmse,      4),
    r_squared = round(r_squared, 4)
  ))
}

top5_results <- top5_results[order(top5_results$aic), ]
print(top5_results)