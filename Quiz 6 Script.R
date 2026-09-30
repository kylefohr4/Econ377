# In OLS, the fitted value Y^i... is the Models Prediction of Y at Xi
# The OLS residual is Yi - Y^i (actual minus fitted)
# A positive residual means OLS Underpredicted Y
# R^2 is the fraction of the sample variation in Y explained by X
# The total sum of squares decomposes as SST=SSE+SSR

# Find OLS Slope given points
# Three x and y values (edit these)
x <- c(1,2,6)
y <- c(10,10,10)

# Closed-form OLS slope: sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
slope <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)

# Intercept (optional)
intercept <- mean(y) - slope * mean(x)

intercept + slope * 7
 

# Three points (edit these)
x <- c(1, 2, 3)
y <- c(2, 4, 5)

# Fitted line: y_hat = intercept + slope * x (edit these)
intercept <- 0.3333
slope     <- 1.5

# Fitted values and residuals
y_hat <- intercept + slope * x
resid <- y - y_hat

# Sum of squared residuals
SSR <- sum(resid^2)
