# Load dataset
df <- read.csv("./spotify.csv")
View(df)

# Question 1a
model = lm(popularity ~ ., data = df)

num_of_breaks = sqrt(length(model$residuals))

par(mfrow = c(1, 3))

windows()
car::qqPlot(model$residuals, pch = 22)

windows()
hist(model$residuals, breaks = num_of_breaks)

# Question 1b
n <- length(model$residuals)
s <- summary(model)

regular_residuals <- model$residuals

hii <- hatvalues(model)

MSE_i = ((n - 2) * (s$sigma)^2 - regular_residuals^2 / (1 - hii)) / (n - 3)

Rstudent_res <- model$residuals / sqrt(MSE_i)
student_res <- rstudent(model)

windows()
plot(model$fitted.values, Rstudent_res, bg = 1)
abline(h = 0)

# Question 1c
num_of_breaks_popularity <- sqrt(length(df$popularity))

windows()
hist(df$popularity, breaks = num_of_breaks_popularity)

# Question 1d
par(mfrow = c(1, 1))

filtered_dataset <- df[df$popularity > 0, ]

model_positive <- lm(popularity ~ ., data = filtered_dataset)

num_of_breaks_positive <- sqrt(length(model_positive$residuals))

windows()
car::qqPlot(model_positive$residuals, pch = 22)

windows()
hist(model_positive$residuals, breaks = num_of_breaks_positive)

n_positive <- length(model_positive$residuals)
s_positive <- summary(model_positive)

regular_residuals_positive <- model_positive$residuals

hii_positive <- hatvalues(model_positive)

MSE_i_positive = ((n_positive - 2) * (s_positive$sigma)^2 - regular_residuals_positive^2 / (1 - hii_positive)) / (n_positive - 3)

Rstudent_res_positive <- model_positive$residuals / sqrt(MSE_i_positive)
student_res_positive <- rstudent(model_positive)

windows()
plot(model_positive$fitted.values, Rstudent_res_positive, bg = 1)
abline(h = 0)

# Question 2a
student_res <- rstudent(model_positive)

covariates <- names(model_positive$model)[-1]

for (covariate in covariates) {
    windows()
    plot(model_positive$model[[covariate]], student_res,
         main = paste("Studentized Residuals vs", covariate))
    abline(h = 0)
}

# Question 2b
model_positive_updated <- lm(popularity ~ danceability + energy + tempo, data = filtered_dataset)

windows()
hist(model_positive_updated$residuals)

windows()
car::qqPlot(model_positive_updated$residuals, pch = 22)

windows()
n_positive_updated <- length(model_positive_updated$residuals)
s_positive_updated <- summary(model_positive_updated)

regular_residuals_positive_updated <- model_positive_updated$residuals

hii_positive_updated <- hatvalues(model_positive_updated)

MSE_i_positive_updated = ((n_positive_updated - 2) * (s_positive_updated$sigma)^2 - regular_residuals_positive_updated^2 / (1 - hii_positive_updated)) / (n_positive_updated - 3)

Rstudent_res_positive_updated <- model_positive_updated$residuals / sqrt(MSE_i_positive_updated)
student_res <- rstudent(model_positive_updated)

windows()
plot(model_positive_updated$fitted.values, Rstudent_res_positive_updated, bg = 1)
abline(h = 0)
