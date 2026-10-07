# To find change in y, multiply b1 and the change in x

# Percent change is (new-old)/old * 100

x <- c(6,6,8)
y <- c(6,1,1)
var(x)
cov(x,y)
mean(x)
sd(x)
cor(x,y)

# Variance of a random sample is E[X^2]-(E[X])^2

# E[Y!X=x] is the average of Y among observations with X=x

#Finding Var(X)

var_X <- 2
a <- 3

# Var(aX + b) = a^2 * Var(X), regardless of b
var_aXb <- a^2 * var_X

# Cov(X,Y) given expected Values

E_XY <- 10
E_X  <- 2
E_Y  <- 3

# Cov(X, Y) = E[XY] - E[X] * E[Y]
cov_XY <- E_XY - E_X * E_Y

#Finding covariance given paired values

x <- c(1, 2, 3)
y <- c(2, 4, 5)
p <- c(0.2, 0.5, 0.3)   # probability of each (x, y) pair

# Probabilities must sum to 1
if (abs(sum(p) - 1) > 1e-8) stop("Probabilities must sum to 1.")

E_X  <- sum(p * x)
E_Y  <- sum(p * y)
E_XY <- sum(p * x * y)

cov_XY <- E_XY - E_X * E_Y

#Finding Correlation given Covariance and sd

cov_XY  <- 0.71
sd_X    <- 0.83
sd_Y    <- 1.22

if (sd_X <= 0 || sd_Y <= 0) stop("Standard deviations must be positive.")

# Corr(X, Y) = Cov(X, Y) / (sd_X * sd_Y)
corr_XY <- cov_XY / (sd_X * sd_Y)

# OLS slope(b1^) = cov(x,y)/var(x)

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

#Covariance based on joint probabilities
x <- c(1, 5, 0)
y <- c(2, 0, 2)
p <- c(0.2, 0.3, 0.5)

if (abs(sum(p) - 1) > 1e-8) stop("Probabilities must sum to 1.")

E_X  <- sum(p * x)
E_Y  <- sum(p * y)
E_XY <- sum(p * x * y)

cov_XY <- E_XY - E_X * E_Y
