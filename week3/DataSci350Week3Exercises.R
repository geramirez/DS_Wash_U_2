x = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=FALSE)
names(x) = c("Sex","Length", "Diameter", "Height", "WholeWeight", "SchuckedWeight", "VisceraWeight", "ShellWeight", "Rings")

qn = 3

q = quantile(x$Height, probs=seq(0,1,1/qn))
H = diff(range(x$Height))
q[[1]] = q[[1]] - 0.1*H
q[[length(q)]] = q[[length(q)]] + 0.1*H
x$Height3 = as.numeric(cut(x$Height, breaks=q))

q = quantile(x$WholeWeight, probs=seq(0,1,1/qn))
H = diff(range(x$WholeWeight))
q[[1]] = q[[1]] - 0.1*H
q[[length(q)]] = q[[length(q)]] + 0.1*H
x$WholeWeight3 = as.numeric(cut(x$WholeWeight, breaks=q))

# joint distribution
j = table(x$Height3, x$WholeWeight3)/nrow(x)
j

# Are height and weight independent?
# Compute the individual distributions (of course, you know what they should be from the way we constructed them)
h = table(x$Height3)/nrow(x)
w = table(x$WholeWeight3)/nrow(x)
h
w

# compute the distribution that would occur if the two were indenpendent
i = outer(h, w)
i

# compare the two distributions in a relative sense
err = (i - j)/i
err
sum(abs(err))/(qn**2) # average relative error of entries in the joint dist.

# compare to a case in which the variables really are independent
N = 128
x = rnorm(N)
y = rexp(N)

qn = 3

q = quantile(x, probs=seq(0,1,1/qn))
H = diff(range(x))
q[[1]] = q[[1]] - 0.1*H
q[[length(q)]] = q[[length(q)]] + 0.1*H
x3 = as.numeric(cut(x, breaks=q))

q = quantile(y, probs=seq(0,1,1/qn))
H = diff(range(y))
q[[1]] = q[[1]] - 0.1*H
q[[length(q)]] = q[[length(q)]] + 0.1*H
y3 = as.numeric(cut(y, breaks=q))

# joint distribution
j = table(x3, y3)/N
j

# Are height and weight independent?
# Compute the individual distributions (of course, you know what they should be from the way we constructed them)
h = table(x3)/N
w = table(y3)/N
h
w

# compute the distribution that would occur if the two were indenpendent
i = outer(h, w)
i

# compare the two distributions in a relative sense
err = (i - j)/i
err
sum(abs(err))/(qn**2) # average relative error of entries in the joint dist.



# The following solutions depend on data computed in the "DataSci350Week3Script.R"
# file. Copy these solutions over to that file at the end of the tweet
# analysis section, then run the entire section.

# Exercise: What is the probability that a tweet reffers to Apple Inc. 
# using the entire sample of tweets?

pb = length(which(labeled.data$label == "yes"))/nrow(labeled.data)
pb

# Exercise: What is the probability that a tweet passes the s2 test
# using the entire sample of tweets?

pa = length(which(labeled.data$s2 == TRUE))/nrow(labeled.data)
pa

# Exercise: What is the probability that a tweet reffers to Apple Inc. 
# given than it has passed the s2 test?

y = labeled.data[labeled.data$s2 == TRUE,]
pba = length(which(y$label == "yes"))/nrow(y)
pba

# Exercise: What is the probability that a tweet passes the s2 test 
# given that it reffers to Apple Inc.? Use Bayes' approach.

# P(s2==TRUE | Yes) = P(Yes | s2==TRUE)P(s2==TRUE)/P(Yes)
pab = pba*pa/pb
pab

# let's double check...
y = labeled.data[labeled.data$label == "yes",]
pab = length(which(y$s2 == TRUE))/nrow(y)
pab

# Exercise: Compute the accuracy, precision, and recall of s2
# To simplify things, let's treat the case s2==TRUE and "not sure" 
# as a false positive and s2==FALSE and "not sure" as a false negative.

TP = intersect(which(labeled.data$label=="yes"), which(labeled.data$s2==TRUE))
FP = intersect(which(labeled.data$label!="yes"), which(labeled.data$s2==TRUE))
TN = intersect(which(labeled.data$label=="no"),  which(labeled.data$s2==FALSE))
FN = intersect(which(labeled.data$label!="no"),  which(labeled.data$s2==FALSE))

tp = length(TP)
fp = length(FP)
tn = length(TN)
fn = length(FN)

# the capitolized versions of these variables are handy if you want to start
# inspecting certain cases, e.g., to example the actual tweets in the case 
#of a false negative.

a = (tp+tn)/(tp+fp+tn+fn)
p = tp/(tp+fp)
r = tp/(tp+fn)

a
p
r

# Exercise: Read in the abalone data. Use the function t.test to perform 
# a t test on a sample of 100 abalone to check the hypothesis that the 
# mean diameter of abalone is 0.4.
x = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=FALSE)
names(x) = c("Sex","Length", "Diameter", "Height", "WholeWeight", "SchuckedWeight", "VisceraWeight", "ShellWeight", "Rings")

hist(x$Diameter)

t.test(sample(x$Diameter, 100), mu=0.4)
