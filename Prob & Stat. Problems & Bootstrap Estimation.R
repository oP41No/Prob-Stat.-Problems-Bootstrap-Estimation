## ----setup, include=FALSE-----------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages, echo = FALSE, warning=FALSE, message = FALSE----
# load the packages for graphing and data wrangling
library(ggplot2)
library(PASWR2)
library(dplyr)
library(lattice)
library(boot)
library(MASS)


## -----------------------------------------------------------
# Number of ways for largest num to be 19 and smallest to be 9
choose(9,2)


## -----------------------------------------------------------
# Number of questions
n <- 5
# Probability of guessing a correct answer
p <- 1/4
# Probability of guessing a wrong answer
q <- 1 - p
# Probability of getting at least 4 correct answers
x <- sum(dbinom(4:5, size = n, prob = p))
# Result
x

# --------------------------------------------

# Number of students
n <- 200
# Probability of getting 4 correct answers
p <- 0.015625
# Num of students (out of 200) getting at least 4 correct answers
x <- n * p
# Result
x


## ----pr 11--------------------------------------------------
1 - ppois(12, 15)
1 - ppois(100, 150)


## ----pr. 18a------------------------------------------------
# Probability that 1st drink sold is 4th drink
dgeom(3, 0.70)


## ----pr. 18b------------------------------------------------
# Prob that 4/10 drinks sold is a soft drink
x <- pbinom(4, 10, 0.7)
y <- pbinom(3, 10, 0.7)
z <- x-y
z


## ----Ex. 4.17-----------------------------------------------
# Probability that a light bulb lasts between 6-15 months
pexp(15, 1/10) - pexp(6, 1/10)

# 95th percentile of the distribution
qexp(0.95, 1/10)

# Probability that a light bulb lasts more than 36 months
1 - pexp(36, 1/10)

# Probability that a light bulb that lasted for 10 months will last more than 25
pexp(25, 1/10, lower = FALSE)/pexp(10, 1/10, lower = FALSE)



## -----------------------------------------------------------
# Approximation of population mean & variance of said mean w/ respect to sample size
SRS2 <- srs(WHEATUSA2004$acres, 4)
xbarSRS2 <- apply(SRS2, 1, mean)
c(mean(xbarSRS2), var(xbarSRS2))

SRS3 <- srs(WHEATUSA2004$acres, 5)
xbarSRS3 <- apply(SRS3, 1, mean)
c(mean(xbarSRS3), var(xbarSRS3))

SRS4 <- srs(WHEATUSA2004$acres, 6)
xbarSRS4 <- apply(SRS4, 1, mean)
c(mean(xbarSRS4), var(xbarSRS4))


## -----------------------------------------------------------
GLUCOSE <- GLUCOSE %>% mutate(DIFF = old-new) # create variable DIFF for the difference between old and new level

(GLUCOSE)

ggplot(data = GLUCOSE, aes(sample = DIFF)) +
  stat_qq() +
  theme_bw()

CI <- t.test(GLUCOSE$DIFF)$conf # use t-test since the sample size is small < 30
CI


## -----------------------------------------------------------
library(PASWR2)
data(STATTEMPS)
#(a)
t.test(temperature ~ gender, data = STATTEMPS, mu = 0, paired = FALSE)
CI <- t.test(temperature ~ gender, data = STATTEMPS, mu = 0, paired = FALSE)$conf

#(b)
t.test(temperature ~ class, data = STATTEMPS, mu = 0, paired = FALSE)
CI <- t.test(temperature ~ class, data = STATTEMPS, mu = 0, paired = FALSE)$conf


## -----------------------------------------------------------
x = c(36,30,37,43,42,43,43,46,40,42)
qqnorm(x) # see if the data appear to follow normal distribution


## -----------------------------------------------------------

n = length(x)
set.seed(25)  

# sample mean
xbar = mean(x)

cat("data mean = ",xbar,'\n')

nboot = 10000 # number of bootstrap samples 

# Generate 10000 bootstrap samples, i.e. an n x 10000 array of random resamples from x.
tmpdata = sample(x,n*nboot, replace=TRUE) # sample with replacement
bootstrapsample = matrix(tmpdata, nrow=n, ncol=nboot)

# Compute the sample mean xbar for each bootstrap sample
xbarstar = colMeans(bootstrapsample)

# Compute delta* for each bootstrap sample (difference between the original mean and the bootstrap sample mean)
deltastar = xbarstar - xbar

# Find the 0.1 and 0.9 quantiles for deltastar
d = quantile(deltastar,c(0.05,0.95)) # for the 905 CI we need 90% area between the quantiles


# Calculate the 90\% confidence interval for the mean.
ci = xbar - c(d[2],d[1])
ci


## -----------------------------------------------------------

MEAN <- function(data, i){
  d <- data[i]
   M <- mean(d)
  }
boot.obj <- boot(x, statistic = MEAN, 10000)
CI <- boot.ci(boot.obj, conf = 0.90,type = "all") # all type of bootstrap intervals will be returned
CI


## -----------------------------------------------------------
data(WCST)

WCST %>% pull(score) %>% eda()


## -----------------------------------------------------------
# CI <- with(data = WCST, t.test(score)$conf) # 

# or using piping
CI <- WCST %>% pull(score) %>% t.test() %>% .$conf
CI


## -----------------------------------------------------------
library(boot) # use boot package
MEAN <- function(data, i){
  d <- data[i]
   M <- mean(d)
  }

set.seed(13)

B <- 10^4 - 1 # number of bootstrap samples

b.obj <- boot(data = WCST$score, statistic = MEAN, R = B) # bootstrap object of class "boot" containing the output of a bootstrap calculation
CIB <- boot.ci(b.obj, conf = 0.95, type = "bca")
CIB


## -----------------------------------------------------------
set.seed(45)

# Set the sample
sample <- rexp(200, rate = 1/4)

library(boot)

# Find the true mean of the sampled distribution
MEAN <- function(data, i){
  d <- data[i]
   M <- mean(d)
}

B <- 10^4-1

b.obj <- boot(data = sample, statistic = MEAN, R = B)
CIB <- boot.ci(b.obj, type = "all")
CIB


## -----------------------------------------------------------
set.seed(45) 

sample <- rbinom(n=25, size=20, prob = 1/5)

library(boot)

MEAN <- function(data, i){
  d <- data[i]
   M <- mean(d)
}

B <- 10^4-1

b.obj <- boot(data = sample, statistic = MEAN, R = B)
CIB <- boot.ci(b.obj, type = "all")
CIB

