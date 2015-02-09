# playing with twitter data
setwd("/home/ramirezg/Documents/UW_DataScience/Semester2/week4")
tweets <- read.csv("labeledDataWithS2.csv", encoding="UTF-8")


# make a contingency table
tweet.table <- table(tweets$s2,tweets$label)
tweet.table <- tweet.table[c(1,3),1:2]

# Prob that is actually refers to apple
p.b <- length(tweets$s2[tweets$label == "yes"])/nrow(tweets)

# Prob that a tweet passes the s2 test using the entire sample of tweets
p.a <- length(tweets$s2[tweets$s2 == TRUE])/nrow(tweets)

# Prob that that a tweet reffers to apple given that it has passed s2
test.passed <- tweets[tweets$s2 == T,]
p.b.a <- nrow(test.passed[test.passed$label=="yes",])/nrow(test.passed)


# Prob that a tweet passes the s2 tests given that it refers to apple inc
lablel.yes <- tweets[tweets$label == "yes",]
nrow(lablel.yes[lablel.yes$s2==TRUE,])/nrow(lablel.yes)

pab = p.b.a*p.a/p.b

# Acurracy, percision, recall
tp = 42
tn = 47
fp = 8
fn = 60

accuracy <- (tp + tn)/(tp + tn + fp + fn)
accuracy
precision <- tp/(tp + fp)
precision
recall <- tp/(tp + fn)
recall
