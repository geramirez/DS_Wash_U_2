rm(list=ls())
setwd("Code/UW_Data_Science/DS_Wash_U_2/week7/hwrk")

data <- read.csv("MobileCarrierHolidayAnalysis2013.csv")

# Methodology look for outliers by marking anybody outside 95% conf. for each attribute as odd.
# Then create an "odd"score. 

# Clean data 
data$PctFirstCDR <- as.numeric(gsub(pattern = "%", replacement = "", x = data$PctFirstCDR))
data$Dealer <- NULL
data <- na.omit(data)

results <- c()
for (var in names(data)){
  odd <- c()
  for ( i in 1:nrow(data) ){
    # getting confidence interval
    u <- mean(data[-c(i), var])
    n <- nrow(data[-c(i),])
    sd <- sd(data[-c(i), var])
    error <- qnorm(0.99)*sd/sqrt(n)
    # getting single mean
    u.single <- mean(data[i, var])
    # Check if it's outside the confidence interval 
    if(error - u < u.single & u.single < error + u) {
      odd <- c(odd, 0)
    }
    else{
      odd <- c(odd, 1)
    }
  }
  results <- cbind(results, odd)
}

data[['percent_odd']] <- rowSums(results)/ncol(results)


##### Enron Data

library(entropy)
library(data.table)

# Load Data
doc.list <- read.csv("doc.list.csv")
enron.data <- data.table(read.table("docword.enron.txt", skip=3))
vocab <- read.csv("vocab.enron.txt", stringsAsFactors = FALSE)

# Subset the data
enron.data.cut  <- enron.data[enron.data$V1 %in% doc.list$x, ]
enron.data <- enron.data[!(enron.data$V1 %in% doc.list$x), ]

# Function
get.score <- function(index) {    
  aT <- nrow(enron.data[V2 == index])
  sT <- nrow(enron.data.cut[V2 == index])
  aF <- length(unique(enron.data$V1)) - aT
  sF <- length(unique(enron.data.cut$V1)) - sT
  x <- matrix(c(aT, aF, sT, sF), nrow=2)
  return( mi.plugin(x))
}

# Calculate scores for words that appear in the subset more than 10 times
word.freq <- sort(table(enron.data.cut$V2), decreasing = T)
word.freq <- word.freq[word.freq > 10]
scores <- sapply(X = as.numeric(names(word.freq)), FUN = get.score) 

# Create a data.frame 
top.words <- data.frame(words = vocab[as.numeric(names(word.freq)), 1],score = scores)
top.words <- top.words[order(-top.words$score), ]

# Get top ten words
top.words[1:10, ]

# Analysis 
# Based on the top words it seems that these emails are about football.
# 'Sportslines' is a the CBS sports site, 'fantz' alludes to fantasy football, 
# and words like 'injuryprone' and 'footballguy' are clearly related to the topic. 

