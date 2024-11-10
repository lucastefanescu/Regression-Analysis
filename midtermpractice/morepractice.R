#3.4 textbook
df = MPV::table.b1

model = lm(y ~ x7 + x8, data = df)
print(summary(model))

p <- length(coef(model))
n <- nrow(df)

dft <- n - 1
dfe <- n - p
dfm <- p - 1

y_bar <- mean(df$y)

SSE <- sum((df$y - predict(model))^2)
SST <- sum((df$y - y_bar)^2)
SSM <- SST - SSE

MSE <- SSE / dfe
MSM <- SSM / dfm

F <- MSM / MSE

coefs <- (n - 1) / (n - p)

R_2 <- SSM / SST
R_2_adj <- 1 - ((1 - R_2) * coefs)
print(F)
print(R_2)
print(R_2_adj)

print(confint(model, level=0.95))

z <- data.frame(x7 = 56.0, x8 = 2100)
print(predict(model, newdata = z, interval = 'confidence'))