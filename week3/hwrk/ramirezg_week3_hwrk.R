setwd("/home/ramirezg/Documents/UW_DataScience/Semester2/week3/hwrk")

# Load Data (using base b/c set is small)
data <- read.csv("birthDataDays.csv")

#### Question 1
# A mother's due date is calculated to be the 280th day of pregnancy. 
# Based on this data set, what is the probability of a baby being born 
# 24 or more days early?

# Combining first and second pregnancies
all.pregnancies <- c(data$First, data$Second)

# calculating mean, sd, and comp. norm dist.
all.mean <- mean(all.pregnancies)
all.sd <- sd(all.pregnancies)
all.norm.dist <- rnorm(n = length(all.pregnancies), 
                       mean = all.mean, sd = all.sd)

# Checking for normality 
hist(all.pregnancies)
qqplot(all.norm.dist, all.pregnancies)
abline(0,1, col="red")
# This dataset appears to be left skewed, and cut off after 280 days

# Finding probability
length(all.pregnancies[all.pregnancies <= 256])/length(all.pregnancies)


# Question 2
# How might the use of this data set bias the result from the last 
# question?

# First, this the data are not a random sample. Also, it appears that the 
# data do not have a near-perfect normal distribution, because as 
# the article state doctors perform c-section pregnancies to 
# continue after the due dates. 

# Suppose that you now learn that the baby in question is a second 
# child. Now what is the probability of our baby being born 24 or more 
# days early?

length(data$Second[data$Second <= 256])/length(data$Second)


# Furthermore, the first child was late. Now that you also know that, 
# what is the probability of the baby being born 24 or more days early?
first.late <- data[data$First > 280, ]
length(first.late$Second[first.late$Second <= 256])/length(first.late$Second)

# Does the length of a pregnancy for a first child follow the same 
# distribution as the length for the second child?

qqplot(x = data$First, y = data$Second)
abline(0, 1, "red")
# Yes

# Is the length of a pregnancy for a first child independent 
# of the length for the second child?

# No they are no independent, there is a trend and when compared 
# an independent dist of the data, the standard error is high. 

# Frist, Computer joint distribution
first.bins <- cut(x = data$First, 
                  quantile(data$First, probs = seq(from = 0,to = 1,length.out = 5)))
second.bins <- cut(x = data$Second, 
                  quantile(data$Second, probs = seq(from = 0,to = 1,length.out = 5)))

joint.table  <- table(first.bins, second.bins)/nrow(data) 

# Second, compute the individual distributions 
first.d <- table(first.bins)/nrow(data)
second.d <- table(second.bins)/nrow(data)

# Compute the distrubtion if both were independent
ind.dist <- outer(first.d, second.d)

# compare the two distributions
err = (ind.dist - joint.table)/ ind.dist
# average rel. error
sum(abs(err))/5




