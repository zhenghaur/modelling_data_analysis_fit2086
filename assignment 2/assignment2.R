# setwd
setwd("C:/Users/lzhau/Documents/Monash Uni/2022S2/FIT2086/assignment2")

# Question 1

# Question 1.1
# sample 1
daily_covid <- read.csv("daily.covid.aug1to7.csv")

# estimate average = sample mean
avg_daily <- mean(daily_covid$daily.covid.cases)
# avg_daily = 7359.571

# sample size
n <- length(daily_covid$daily.covid.cases)
# n = 7

# sample variance
var_daily <- var(daily_covid$daily.covid.cases)
# var_daily = 4108400

# standard error
se <- sqrt(var_daily)/sqrt(n)
# se = 766.1033

# t-score (a = 0.05, dof = n-1)
t <- qt(p = 1-0.05/2, df = n-1)
# t = 2.446912

# 95% CI for this estimate using the t-distribution
CI <- c(avg_daily - t*se, avg_daily + t*se)
# CI = [5484.984, 9234.159]

# Question 1.2
# sample 2
daily_covid_2 <- read.csv("daily.covid.aug8to14.csv")

# estimate average = sample mean
avg_daily_2 <- mean(daily_covid_2$daily.covid.cases)
# avg_daily_2 = 4879

# mean difference
mean_diff <- avg_daily - avg_daily_2
# mean_diff = 2480.571

# sample variance
var_daily_2 <- var(daily_covid_2$daily.covid.cases)
# var_daily_2 = 1286109

# sample size
n_2 <- length(daily_covid_2$daily.covid.cases)
# n_2 = 7

# standard error 
se_diff <- sqrt(var_daily/n + var_daily_2/n_2)
# se_diff = 877.8634

# 95% CI of difference of means
CI <- c(mean_diff - 1.96*se_diff, mean_diff + 1.96*se_diff)
# CI = [759.9591, 4201.1837]

# Question 1.3
# z-score 
z <- mean_diff/se_diff
# z = 2.825692

# p-value
p <- 2 * pnorm(-abs(z))
# p = 0.004717864

# Question 2

# Question 2.1 
# negative binomial probability mass function 
prob_mass_func = function(y, v, r) {choose(y+r-1, y) * r^r * (exp(v)+r)^(-r-y) * exp(y*v)}

# plotting graph
plot(prob_mass_func((0:25), 0, 1), type = 'o', lwd = 2, col = 1, pch = 4, main = "Negative Binomial Probability Mass Function", xlab = "y", ylab = "p(y | v, r)")
lines(prob_mass_func((0:25), 1, 2), type = 'o', lwd = 2, col = 2, pch = 4)
lines(prob_mass_func((0:25), 1.5, 2), type = 'o', lwd = 2, col = 3, pch = 4)
legend(x="topright",legend = c("v = 0, r = 1","v = 1, r = 2","v = 1.5, r = 2"), fill=c(1, 2, 3))

# Question 3

# Question 3.1
# sample size
n <- 240

# observed turn right
m <- 176

# maximum likelihood estimator
theta_hat <- m/n
# theta = 0.7333333

# 95% CI for the probability parameter theta
CI <- c(theta_hat - 1.96*sqrt((theta_hat*(1-theta_hat))/n), theta_hat + 1.96*sqrt((theta_hat*(1-theta_hat))/n))
# CI = [0.6773852, 0.7892815]

# Question 3.2
# sample size
n <- 240

# observed turn right
m <- 176

# maximum likelihood estimator
theta_hat  <- m/n
# theta = 0.7333333

# theta null (no preference)
theta_0 <- 1/2

# z-score
z <- (theta_hat-theta_0) / sqrt(theta_0*(1-theta_0)/n)
# z = 7.229569

# p-value
p <- 2 * pnorm(-abs(z))
# p = 4.845296e-13

# Question 3.3
# sample size
n <- 240

# observed turn right
m <- 176
binom.test(x = m, n = n, p = 1/2)
# p-value of 2.854e-13



