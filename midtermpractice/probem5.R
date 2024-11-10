df = MPV::table.b3

model = lm(y ~ x1 + x6, data = df)

n = nrow(df)
p = length(coef(model))

y_bar <- mean(df$y)

dft <- n - 1
dfm <- p - 1
dfe <- n - p

SST <- sum((df$y - y_bar)^2)
SSE <- sum((df$y - predict(model))^2)
SSM <- SST - SSE

MSM <- SSM / dfm
MSE <- SSE / dfe

F <- MSM / MSE

p.val <- 1 - pf(F, dfm, dfe)

ANOVA_TABLE = rbind(c(SSM, dfm, MSM, F, p.val), c(SSE, dfe, MSE, NA, NA), c(SST, dft, NA, NA, NA))
rownames(ANOVA_TABLE) = c("Model", "residual", "total")
colnames(ANOVA_TABLE) = c("SS", "df", "MS", "F", "p-val")

print(ANOVA_TABLE)

R_2 <- SSM / SST
coefs <- (n - 1) / (n - p)
R_2_adj <- 1 - ((1 - R_2) * coefs)

print(R_2)
print(R_2_adj)
print(summary(model))

print(confint(model, level=0.95))

z <- data.frame(x1 = 275, x6 = 2)

predicted_values <- predict(model, newdata = z, interval = 'confidence', level=0.95)
predicted_interval <- predict(model, newdata = z, interval = 'prediction', level=0.95)
print(predicted_interval)
print(predicted_values)

