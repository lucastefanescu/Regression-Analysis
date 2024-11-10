# install.packages('MPV')

df = MPV::table.b1

model = lm(y ~ x2 + x7 + x8, data = df)
print(summary(model))


p = length(coef(model))
n <- nrow(df)

dfm <- p - 1
dfe <- n - p
dft <- n - 1

Y <- as.matrix(df$y)                  
X <- model.matrix(model)               
beta_hat <- coef(model)                
Y_hat <- X %*% beta_hat                
res <- Y - Y_hat                
SSE_matrix <- t(res) %*% res
SST_matrix <- t(Y - mean(Y) * rep(1,n)) %*% (Y-mean(Y) * rep(1,n))
y_bar <- mean(df$y)

SSE <- sum((df$y - predict(model))^2)
SST <- sum((df$y - y_bar)^2)
SSM <- SST - SSE

MSM <- SSM / dfm
MSE <- SSE / dfe

Fv <- MSM / MSE

p.val = 1 - pf(Fv, dfm, dfe)

# print(Fv)
# print(dfm)
print(dfe)

ANOVA_TABLE = rbind(c(SSM, dfm, MSM, Fv, p.val), c(SSE, dfe, MSE, NA, NA), c(SST, dft, NA, NA, NA))
rownames(ANOVA_TABLE) = c("Model", "residual", "total")
colnames(ANOVA_TABLE) = c("SS", "df", "MS", "F", "p-val")

# print(ANOVA_TABLE)

R_2 <- SSM / SST

coefs <- (n - 1) / (n - p)

R_2_adj <- 1 - ((1 - R_2) * coefs)

# print(R_2)
# print(R_2_adj)

model_reduced = lm(y ~ x2 + x8, data = df)

SSE_R <- sum((df$y - predict(model_reduced))^2)

numerator <- SSE_R - SSE

p_new <- length(coef(model_reduced))

dfe_diff <- (n-p_new) - dfe

MS_drop <- numerator / (dfe_diff)

F_partial <- MS_drop / MSE

# print(F_partial)

r <- cor(df$y, predict(model))

# print(r)

# print(confint(model, level=0.95))

z <- data.frame(x2 = 2300, x7 = 56.0, x8 = 2100)

predicted_values <- predict(model, newdata = z, interval = 'confidence')

print(predicted_values)