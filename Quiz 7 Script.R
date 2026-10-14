# 1 - test score = b0 + b1classsize + U, zero conditional mean assumption fails because avg U changes with class size

# 2 - OLS estimates b0hat, b1hat are least squares estimates; residuals must equal 0, OLS must be smallest 

# 3 - var(X) = E[X]^2 and sd(aX + b) = |a|sd(X); given E[X] and E[X]^2, find sd of 3X - 4
EX <- 4
EX2 <- 20
varx <- EX2 - EX^2
sd_3X_minus4 <- abs(3) * sqrt(varx)
sd_3X_minus4

# 4 - var(aX + bY) = a^2var(X) + b^2var(Y) + 2abcov(x, y); given var(X) and cov(x, y), find var(2X - 3Y)
varx <- 8
vary <- 5
covxy <- -1.5
var_2x_minus_3y <- 2^2 * varx + (-3)^2 * vary + 2 * 2 * (-3) * covxy
var_2x_minus_3y

# 5 - find cor(X, Y) given join distribution; cov(x, y) = E[XY] - E[X] * E[Y], cor(x, y) = cov(x, y) / (sd(X) * sd(Y))
x <- c(2, 4, 5, 8)
y <- c(2, 7, 4, 6)
p <- c(0.2, 0.3, 0.4, 0.1)
EX <- sum(x * p)
EY <- sum(y * p)
EXY <- sum(x * y * p)
covxy <- EXY - EX * EY
varx <- sum(x^2 * p) - EX^2 
vary <- sum(y^2 * p) - EY^2
corxy <- covxy / sqrt(varx * vary)
corxy

# 6 - for sample x, regress y on x by OLS and find the residual of the third observation
x <- c(4, 6, 7, 8, 10)
y <- c(4, 2, 10, 6, 6)
b1 <- cov(x, y) / var(x)
b0 <- mean(y) - b1 * mean(x)
y[3] - (b0 + b1 * x[3])

# 7 - OLS regression on six observations given 5 residuals, find the sixth 
res <- c(2.1, 0.4, 1.1, 2.1, 0.3)
-sum(res)

# 8 - for sample x, regress y on x by OLS and compute the summation xiuhati; OLS property says always = 0 
x <- c(1, 4, 6, 8, 13, 15)
y <- c(9, 7, 8, 7, 10, 3)
sum(x * resid(lm(y ~ x)))

# 9 - find SSR given SSE and R^2; R^2 = SSE / SST gives SST, SST = SSE + SSR
SSE <- 162
R2 <- 0.88
SSR <- SSE / R2 - SSE
SSR

# 10 - find total SST given n and sample variance sy^2
n <- 17
sy2 <- 19
SST <- sy2 * (n - 1)
SST

# 11 - what is the oLS residual of worker in row 12
data(wage1)
reg <- lm(wage ~ tenure, data = wage1)
reg$residuals[12]

# 12 - find sample mean of y given OLS regression intercept b0, slope b1, and sample mean xbar
b0 <- 6
b1 <- 1.2
xbar <- 9
ybar <- b0 + b1 * xbar
ybar

# 13 - find new OLS slope given b1, every value multiplied by 10
beta1 <- 1.95
new_beta1 <- beta1 * 10
new_beta1

# 14 - find new OLS slope given b1, every value divided by 1000
beta1 <- 0.14
new_beta1 <- beta1 * 1000
new_beta1

# 15 - in a regression of y on x, every value of x is multiplied by 10; means slope is divided by 10, intercept and R^2 do not change

# 16 - if salary measured in $, what is OLS slope
data("ceosal1")
reg1 <- lm(salary ~ sales, data = ceosal1)
reg1$coefficients[2] * 1000

# 17 - find what percent y changes when x rises by 1 given log(y)hat 
beta1 <- 0.02
pct_change <- 100 * beta1 * 1
pct_change

# 18 - find what percent y changes when x rises by 13% given log(y)hat
beta1 <- 1.18
pct_x <- 13
pct_y <- beta1 * pct_x
pct_y

# 19 - by how many units does y change when x rises by 17% given yhat
beta1 <- 57.2
pct_x <- 17
deltay <- (beta1 / 100) * pct_x
deltay

# 20 - given log(rent)hat = 2.1 + 0.45log(income), the 0.45 means 1% rise in income is associated with a 0.45% rise in rent

# 21 - by what percent does predicted wage change with 10 more years of experience; use wage1
data("wage1")
reg1 <- lm(log(wage) ~ exper, data = wage1)
answer <- reg1$coefficients[2] * 100 * 10
answer