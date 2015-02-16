# http://www.cyclismo.org/tutorial/R/pValues.html

#prepare system
setwd("/home/ramirezg/Documents/UW_DataScience/Semester2/week2")
library(data.table)
clean_names <- function (name) {  
  name <- tolower(name)
  name <- gsub(pattern = "[^[:alpha:]]", replacement = " ",x = name)
  name <- strsplit(x = name, split = " ")[[1]][1]
  return (name)
}
data <- fread("weather_data_2006_2009.csv")
setnames(x=data,
         old=names(data),
         new=sapply(names(data), FUN=clean_names, USE.NAMES=F))
data[,pres:=NULL] #remove extra column
data <- data[temp > 0, ]

# Get sample
temperature = sample(data[, temp], 10000)

# Get measurments
mu = mean(temperature)
s = sd(temperature)

# look at distrubution
hist(temperature, freq=F, xlim=c(0,100))
par(new=T)
plot(function(x) dnorm(x, mean=mu, sd=s), col='red')

# Does the mean temperature equal 51 degrees?
t.test(x=temperature, mu = 51)
# We reject the null hypothesis that that the temp.
# Is not 51 degrees

# Is the mean temperature less than 51 degrees?
t.test(x=temperature, mu=51, alternative = "less")
t.test(x=temperature, mu=51, alternative = "greater")

for (i in 30:70){
  print(i)
  print(t.test(x=temperature, mu = i)$p.value )
}

# Is the mean temperature in January significantly 
# different from the mean temperature in April?
temperatureJan <- sample(data[month==1, temp], size = 10000)
temperatureApr <- sample(data[month==4, temp], size = 10000)
t.test(x = temperatureJan, y = temperatureApr)

# do a power analysis on the original temp samples
# It is the probability that you are right, when the null hypo is true
power.t.test(n=100, delta = 1, sd= sd(temperature), sig.level = 0.05, power=NULL)

# how much smaller could the sample be in order to read the .8 power
power.t.test(n=NULL, delta = 1, sd= sd(temperature), sig.level = 0.05, power=.8)


# playing with twitter data
setwd("/home/ramirezg/Documents/UW_DataScience/Semester2/week4")
tweets <- read.csv("labeledDataWithS2.csv", encoding="UTF-8")

# make a contingency table
tweet.table <- table(tweets$label, tweets$s2)
tweet.table <- tweet.table[c(1,3),1:2]

chisq.test(x = tweet.table)
# if you had a very small sample set you should use fisher's exact test

# Compute probablity that tweet refers to apple
pb = length(which(tweets$label == "yes"))/nrow(tweets)

# Compute prop that tweet passes s2
pa = length(which(tweets$s2 == TRUE))/nrow(tweets)

# Compute prop that tweet reffers to apple give than it passed s2
# reducing the sample size
y = tweets[tweets$s2 == TRUE,]
pba = length(which(y$label == "yes"))/nrow(y)

# What is the prob that a tweet passes the s2 test given that it reffers to apple
# P(s2==TRUE | yes) = P(yes | s2==TRUE) P(s2=TRUE)/P(yes)
pab = pba*pa/pb

y = tt
# Accuracy percision and recall 
TP = 42
FP = 8
TN = 47
FN = 60

a = (TP+TN)/(TP + FP + TN + FN)
p = TP/(TP + FP)
r = TP/(TP + FN)
a 
p 
r