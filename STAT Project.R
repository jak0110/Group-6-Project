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

# Box plots of each numeric variable
for (num in numlist) {
  boxplot(spotify_data[[num]],
          main = paste("Boxplot of", num),
          ylab = num,
          col  = "steelblue",
          outline = TRUE
  )
}


#Bar charts for popularity biased on explicit and major/minor key
for (cat in c("explicit", "mode")) {
  categories <- unique(spotify_data[[cat]])
  for (level in categories) {
    subset_data <- spotify_data$popularity[spotify_data[[cat]] == level]
    bins <- cut(subset_data, breaks = seq(0, 100, by = 10), include.lowest = TRUE)
    counts <- table(bins)
    barplot(counts,
            main = paste("Popularity -", cat, ":", level),
            xlab = "Popularity",
            ylab = "Count"
    )
  }
}
#Table of count, median, and mean popularity by genre
genre_stats <- data.frame(
  genre  = unique(spotify_data$track_genre),
  count  = tapply(spotify_data$popularity, spotify_data$track_genre, length),
  median = tapply(spotify_data$popularity, spotify_data$track_genre, median),
  mean   = tapply(spotify_data$popularity, spotify_data$track_genre, mean)
)
genre_stats <- genre_stats[order(genre_stats$mean, decreasing = TRUE), ]
print(genre_stats)
# Take only main artist
split_artists <- strsplit(as.character(spotify_data$artists), ";")
main_artist <- data.frame(
  artist     = trimws(sapply(split_artists, `[`, 1)),
  popularity = spotify_data$popularity
)
# Build stats table
artist_stats <- data.frame(
  count  = tapply(main_artist$popularity, main_artist$artist, length),
  median = tapply(main_artist$popularity, main_artist$artist, median),
  mean   = tapply(main_artist$popularity, main_artist$artist, mean)
)
artist_stats$name <- rownames(artist_stats)
artist_stats <- artist_stats[order(artist_stats$mean, decreasing = TRUE), ]
artist_stats <- artist_stats[-4]
#Only look at artists with 10 songs
artist_stats <- artist_stats[artist_stats$count > 10,]
print(artist_stats)
#Correlation plots of popularity vs each numeric variable
for (num in numlist) {
  if (num != "popularity") {
    plot(spotify_data[[num]], spotify_data$popularity,
         main = paste("Popularity vs", num),
         xlab = num,
         ylab = "Popularity",
         col = "steelblue",
         pch = 16)
    abline(lm(spotify_data$popularity ~ spotify_data[[num]]), col = "red", lwd = 2)
  }
}

