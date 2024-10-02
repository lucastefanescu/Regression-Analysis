#### Name: Luca Stefanescu
#### Student Number: 219004852
#### Math 3330 Assignment 1

#Q1 a) see pdf
#Q1 b):

DF <- read.csv("./spotify.csv") # load dataset
View(DF) #View dataset

model = lm(popularity ~ duration_ms + danceability + energy + acousticness + instrumentalness + liveness + tempo, data = DF)

print(summary(model))
# 1 c) see pdf

# 1 d) see pdf for Interpretation of result

Y <- DF$popularity
X <- model.matrix(~ duration_ms + danceability + energy + acousticness + instrumentalness + liveness + tempo, data = DF)
n <- nrow(X)
p <- ncol(X)

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y

SST <- t(Y - mean(Y) * rep(1, n)) %*% (Y - mean(Y) * rep(1, n))
Yhat <- X %*% beta_hat
res <- Y - Yhat
SSE <- t(res) %*% res
SSM <- SST - SSE

dfm <- p - 1
dfe <- n - p

MSM <- SSM / dfm
MSE <- SSE / dfe

Fv <- MSM / MSE

p.val <- 1 - pf(Fv, dfm, dfe)

ANOVA_Table <- rbind(
  c(SSM, dfm, MSM, Fv, p.val),
  c(SSE, dfe, MSE, NA, NA),
  c(SST, n - 1, NA, NA, NA)
)
rownames(ANOVA_Table) <- c("Model", "Error", "Total")
colnames(ANOVA_Table) <- c("SS", "df", "MS", "F", "p-value")

# ANOVA_Table

# 1 e) see pdf
# 1 f) see pdf
# 1 g) see pdf

#1 h) see pdf for interpretation
print(confint(model, level=0.95))

# 1 i) see pdf for interpretation

model2 = lm(popularity ~ duration_ms + danceability + energy + instrumentalness + liveness + tempo, data = DF)
print(summary(model2))

# 1 j) see pdf for interpretation

sigma_squared <- MSE / dfe
cat(sprintf("Sigma squared is: %.12f\n", sigma_squared))

# 1 k) / l see pdf for interpretation 

new_song <- data.frame(
  duration_ms = 205594.00,
  danceability = 0.44,
  energy = 0.63,
  acousticness = 0.43,
  instrumentalness = 0.0042,
  liveness = 0.07,
  tempo = 78.90
)

predicted_popularity <- predict(model, newdata = new_song, interval = "confidence", level=0.90)
print(predicted_popularity)