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

# Prepare data with categorical variables
lm_cat_vars <- c("popularity", "duration_ms", "danceability", "energy",
                 "loudness", "speechiness", "acousticness", "instrumentalness",
                 "liveness", "valence", "tempo", "explicit", "mode", "track_genre")

lm_cat_data <- spotify_data[, lm_cat_vars]

# Encode as factors
lm_cat_data$explicit    <- as.factor(lm_cat_data$explicit)
lm_cat_data$mode        <- as.factor(lm_cat_data$mode)
lm_cat_data$track_genre <- as.factor(lm_cat_data$track_genre)

# Train/test split (80/20)
set.seed(123)
train_index  <- sample(seq_len(nrow(lm_cat_data)), size = 0.8 * nrow(lm_cat_data))
lm_cat_train <- lm_cat_data[train_index, ]
lm_cat_test  <- lm_cat_data[-train_index, ]

cat("Training rows:", nrow(lm_cat_train), "\n")
cat("Testing rows: ", nrow(lm_cat_test),  "\n")

# Gen1 models with genre included
gen1_cat_stats <- data.frame(
  variable = character(),
  p_value  = numeric(),
  aic      = numeric(),
  rmse     = numeric()
)

for (num in numlist) {
  if (num != "popularity") {
    formula <- as.formula(paste("popularity ~", num, "+ track_genre"))
    model   <- lm(formula, data = lm_cat_train)
    
    coef_table <- summary(model)$coefficients
    p_val      <- ifelse(num %in% rownames(coef_table), coef_table[num, 4], NA)
    aic        <- AIC(model)
    
    preds  <- predict(model, newdata = lm_cat_test)
    rmse   <- sqrt(mean((lm_cat_test$popularity - preds)^2))
    
    gen1_cat_stats <- rbind(gen1_cat_stats, data.frame(
      variable = num,
      p_value  = p_val,
      aic      = aic,
      rmse     = rmse
    ))
  }
}

gen1_cat_stats <- gen1_cat_stats[order(gen1_cat_stats$aic), ]
print(gen1_cat_stats)

# Filter to top 50% AIC
median_aic_cat <- median(gen1_cat_stats$aic)
top_cat_vars   <- gen1_cat_stats$variable[gen1_cat_stats$aic <= median_aic_cat]

cat("Top 50% AIC variables:", paste(top_cat_vars, collapse = ", "), "\n")

# Generate all pairwise combinations
combos_cat <- combn(top_cat_vars, 2, simplify = FALSE)

# Second generation models with genre always included
gen2_cat_stats <- data.frame(
  variables = character(),
  p_value1  = numeric(),
  p_value2  = numeric(),
  aic       = numeric(),
  rmse      = numeric()
)

for (combo in combos_cat) {
  var1 <- combo[1]
  var2 <- combo[2]
  
  formula <- as.formula(paste("popularity ~", var1, "+", var2, "+ track_genre"))
  model   <- lm(formula, data = lm_cat_train)
  
  coef_table <- summary(model)$coefficients
  p_val1     <- ifelse(var1 %in% rownames(coef_table), coef_table[var1, 4], NA)
  p_val2     <- ifelse(var2 %in% rownames(coef_table), coef_table[var2, 4], NA)
  aic        <- AIC(model)
  
  preds  <- predict(model, newdata = lm_cat_test)
  rmse   <- sqrt(mean((lm_cat_test$popularity - preds)^2))
  
  gen2_cat_stats <- rbind(gen2_cat_stats, data.frame(
    variables = paste(var1, "+", var2, "+ track_genre"),
    p_value1  = p_val1,
    p_value2  = p_val2,
    aic       = aic,
    rmse      = rmse
  ))
}

gen2_cat_stats <- gen2_cat_stats[order(gen2_cat_stats$aic), ]
print(gen2_cat_stats)

# Top 5 gen2 models evaluated on test data
top5_gen2_cat <- head(gen2_cat_stats, 5)

top5_gen2_results <- data.frame(
  variables = character(),
  aic       = numeric(),
  rmse      = numeric(),
  r_squared = numeric()
)

for (i in seq_len(nrow(top5_gen2_cat))) {
  vars <- trimws(unlist(strsplit(as.character(top5_gen2_cat$variables[i]), "\\+")))
  var1 <- vars[1]
  var2 <- vars[2]
  
  formula <- as.formula(paste("popularity ~", var1, "+", var2, "+ track_genre"))
  model   <- lm(formula, data = lm_cat_train)
  
  preds   <- predict(model, newdata = lm_cat_test)
  actuals <- lm_cat_test$popularity
  
  aic       <- AIC(model)
  rmse      <- sqrt(mean((actuals - preds)^2))
  ss_res    <- sum((actuals - preds)^2)
  ss_tot    <- sum((actuals - mean(actuals))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  top5_gen2_results <- rbind(top5_gen2_results, data.frame(
    variables = top5_gen2_cat$variables[i],
    aic       = round(aic,       2),
    rmse      = round(rmse,      4),
    r_squared = round(r_squared, 4)
  ))
}

top5_gen2_results <- top5_gen2_results[order(top5_gen2_results$aic), ]
print(top5_gen2_results)
