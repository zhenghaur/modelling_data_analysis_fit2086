# setwd and read file
setwd("C:/Users/lzhau/Documents/Monash Uni/2022S2/FIT2086/assignment 1")
dog_bites <- read.csv("dogbites.1997.csv")

# 1 
lambda <- mean(dog_bites$daily.dogbites)
lambda
# estimated rate of dog bites per day = 4.391534

# 2a
less_than_2 <- ppois(2, lambda)
less_than_2
# probability of two or less dog bites = 0.1861507

# 2b
dpois_dog_bites <- dpois(0:max(dog_bites), lambda)

# finding 2 highest probability
highest <- sort(dpois_dog_bites)[length(dpois_dog_bites)]
highest2 <- sort(dpois_dog_bites)[length(dpois_dog_bites)-1]

# finding the index of the 2 highest probability
which(dpois_dog_bites == highest)-1
which(dpois_dog_bites == highest2)-1
# the two most likely number of dog bites = 4 and 3

# 2c
less_than_32 <- ppois(32, lambda*7)
less_than_32
# probability of at most 32 dog bites in a week = 0.6346646

# 2d
more_than_3 <- 1 - ppois(2, lambda)
less_than_12_of_14 <- pbinom(11, 14, more_than_3)
more_than_12_of_14 <- 1 - less_than_12_of_14
more_than_12_of_14
# probability of more than 3 dog bite at least 12 of 14 days = 0.5012691

# 3
# plotting a histogram
hist(dog_bites$daily.dogbites, breaks = 22, freq = F, xlim = c(0,22), ylim = c(0,0.4), xlab = "k", ylab = "P(K=k)", main = "Poisson Distribution")
# adding the poisson distribution line
lines(x = xpois, y = dpois(x = xpois, lambda), type = "l", col = "blue") 
lines(x = xpois, y = dpois(x = xpois, lambda), type = "p", col = "blue") 




