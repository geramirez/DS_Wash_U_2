# peg board simulation in the style of coin flips
# write a one-liner to find the location of the ball after it falls 10 levels



# repeat the experiment 1000 times and plot a histogram of the results



# modify the experiment to use exponential jumps
data = c()
for (i in 1:1000){
  data <- c(data, sum(rexp(10,1)))
}
hist(data)
smpl = data


# Testing to see if the distribution is normal
# first, get random data form a real normal distribution
m = mean(smpl)
s = sd(smpl)
y = rnorm(10000,m,s)

# qq-plot
qqplot(smpl,y)
abline(0,1,col='red')

# KS test
ks.test(smpl, y)

# Examine the empirical CDF vs. theoretical CDF
px=seq(0,30,0.2)
py=pnorm(px,m,s)
xl = c(0,30)
yl = c(0,1)
plot(ecdf(smpl), xlim=xl, ylim=yl)
par(new=TRUE)
plot(px,py,col='red', xlim=xl, ylim=yl)

# Back to the peg-board example...
# 50 level peg-board repeated 1000 times
p = 0.5
x = rbinom(10000,50,p)
y = x/50

# Plot a histogram
# note that we've switch from the sum of the "hops" to the average 
hist(y, freq=FALSE, breaks=30, xlim=c(0,1), ylim=c(0,6))
par(new=TRUE)



# EXERCISE: 
# Find missing paramers "clt_XX" to plot the CLT 
# distribution over the empirical distribution
clt_mu = p
clt_sd = sqrt(p*(1-p)/50)
plot(function(x) dnorm(x, clt_mu, clt_sd),xlim=c(0,1), ylim=c(0,6))



# Exercise:
# Predict the outcome of the election by polling the voters
# represented in "votes.csv"
setwd('/home/ramirezg/Documents/UW_DataScience/Semester2/week5')
votes <- read.csv("votes.csv")

moe <- function(n) {
  return(.98/sqrt(n))
}



# 1. Estimate the number of people you need to poll to 
# reach a 5% MOE
(.98/.05)^2

# 2. Take a sample of this many votes and find the mean
# Can you call the election? If not, try again with 
# larger samples until you can (while thinking about
# what this is doing to your phone bill).
poll_1 <- sample(x = votes$vote, size = 400, replace = FALSE)
p <- mean(poll_1) 
moe(400)

# confidence interval 
p + .05
p - .05

# Chi-squared example over cencus data
# Get the data
x = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header=FALSE)
names(x) = c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capitol-loss", "hours-per-week", "native-country", "income")
summary(x)

# Create an indicator variable for reporting
x$"no-report" = (x$"native-country" == " ?")

# Table of results
t  = table(x$"no-report", x$"income")
t

# Compute the row and column sums
nr = margin.table(t, 1)
nr

n5 = margin.table(t, 2)
n5

# Total number of observations
n = sum(t)
n

# P(>50 | R)
t[1,2]/nr[1]

# P(>50 | !R)
t[2,2]/nr[2]

# Class probabilities
pr = nr/n
p5 = n5/n

pr
p5

# Expected probabilities of binary classes
pe = outer(pr, p5)
pe

# Expected counts of binary classes
ne = outer(pr, p5)*n
ne

# Deviation of observed table from expected table under independence assumption
t - ne

# The Chi-squared test (note how the value of chisq changes if the continuity correction is applied)
chisq.test(t, correct=FALSE)

# The Chi-squared distribution and 95% marker
plot(function(x) dchisq(x, 1), xlim=c(0,5))
abline(v=qchisq(.95,1), col="red")

# Size of the deviation we'd need to observe to reject the null hypothesis
sqrt(qchisq(.95,1)/sum(1/ne))

