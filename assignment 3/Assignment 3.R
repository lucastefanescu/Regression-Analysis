##Question 1 a)

df <- read.csv("./spotify.csv")

filtered_df <- df[df$popularity != 0, ]

model <- lm(popularity~.,data=df[df$popularity!=0,])

hat_values <- hatvalues(model)

max_hii_index <- which.max(hat_values)
max_hii_value <- max(hat_values)

n <- nrow(df)
p <- length(coefficients(model))
threshold <- 2 * p / n

proportion <- mean(hat_values > threshold)

cat("Index of largest hii:", max_hii_index, "\n")
cat("Largest hii value:", max_hii_value, "\n")
cat("Proportion of hii > 2p/n:", proportion, "\n")

#Question 1b)

cooks_distances <- cooks.distance(model)

max_cooks_index <- which.max(cooks_distances)
max_cooks_value <- max(cooks_distances)

proportion_greater_1 <- mean(cooks_distances > 1)

cat("Observation with largest Cook's distance:", max_cooks_index, "\n")
cat("Largest Cook's distance value:", max_cooks_value, "\n")
cat("Proportion of Cook's distances > 1:", proportion_greater_1, "\n")

# Question 1c)

X <- model.matrix(model)
X_with_popularity <- cbind(filtered_df$popularity, X)
compute_depth <- function(X) {
  n <- nrow(X)
  total_sum <- colSums(X)
  depths <- numeric(n)
  for (i in 1:n) {
    x_i <- X[i, ]
    sum_excl_i <- total_sum - x_i
    mean_excl_i <- sum_excl_i / (n - 1)
    direction <- x_i - mean_excl_i
    distance <- sqrt(sum(direction^2))
    if (distance == 0) {
      depths[i] <- Inf
    } else {
      depths[i] <- 1 / distance
    }
  }
  return(depths)
}
depth_values <- compute_depth(X_with_popularity)
min_depth_index <- which.min(depth_values)
min_depth_value <- depth_values[min_depth_index]
proportion_less_015 <- mean(depth_values < 0.015)
cat("Observation with smallest depth value:", min_depth_index, "\n")
cat("Smallest depth value:", min_depth_value, "\n")
cat("Proportion of depth values < 0.015:", proportion_less_015, "\n")

#question 1d)

library(ggplot2)

filtered_df$marked <- ifelse(seq_len(nrow(filtered_df)) == min_depth_index, "Low Depth/High Hii", "Other")

windows()
print(
  ggplot(filtered_df, aes(x = liveness, y = duration_ms)) +
    geom_point(data = subset(filtered_df, marked == "Other"), color = "blue", alpha = 0.7) +
    geom_point(data = subset(filtered_df, marked == "Low Depth/High Hii"), color = "red", size = 3) +
    labs(title = "Liveness vs Duration (ms)", x = "Liveness", y = "Duration (ms)") +
    theme_minimal()
)

windows()
print(
  ggplot(filtered_df, aes(x = instrumentalness, y = tempo)) +
    geom_point(data = subset(filtered_df, marked == "Other"), color = "blue", alpha = 0.7) +
    geom_point(data = subset(filtered_df, marked == "Low Depth/High Hii"), color = "red", size = 3) +
    labs(title = "Instrumentalness vs Tempo", x = "Instrumentalness", y = "Tempo") +
    theme_minimal()
)

windows()
print(
  ggplot(filtered_df, aes(x = acousticness, y = energy)) +
    geom_point(data = subset(filtered_df, marked == "Other"), color = "blue", alpha = 0.7) +
    geom_point(data = subset(filtered_df, marked == "Low Depth/High Hii"), color = "red", size = 3) +
    labs(title = "Acousticness vs Energy", x = "Acousticness", y = "Energy") +
    theme_minimal()
)

windows()
print(
  ggplot(filtered_df, aes(x = danceability, y = duration_ms)) +
    geom_point(data = subset(filtered_df, marked == "Other"), color = "blue", alpha = 0.7) +
    geom_point(data = subset(filtered_df, marked == "Low Depth/High Hii"), color = "red", size = 3) +
    labs(title = "Danceability vs Duration (ms)", x = "Danceability", y = "Duration (ms)") +
    theme_minimal()
)

# question 2 a)

df_2 <- read.csv("./clean_data(1).csv")

filtered_data <- df_2[
    df_2$Lotsize > 0 &
    df_2$Sale_price >= 10000 &
    df_2$Fin_sqft >= 500, 
]

filtered_data$ppsq <- log(filtered_data$Sale_price / filtered_data$Fin_sqft)

model <- lm(ppsq ~ log(Lotsize) + Sale_date + Year_Built + as.factor(District) + Bdrms, data = filtered_data)

# question 2b)

if (!require(car)) install.packages("car")
library(car)

vif_values <- vif(model)

print(vif_values)

# question 2c)

if (!require(leaps)) install.packages("leaps")
library(leaps)

all <- leaps::regsubsets(
  ppsq ~ log(Lotsize) + Sale_date + Year_Built + as.factor(District) + Bdrms,
  data = filtered_data,
  nvmax = 10,
  method = 'exhaustive'
)

windows()
plot(all, scale = 'adjr2', main = "All-Subsets Regression with adjR2 Metric")