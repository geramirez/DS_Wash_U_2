#prepare system
setwd("/home/ramirezg/Documents/UW_DataScience/Semester2/week1")
library(data.table)
clean_names <- function (name) {  
  name <- tolower(name)
  name <- gsub(pattern = "[^[:alpha:]]", replacement = " ",x = name)
  name <- strsplit(x = name, split = " ")[[1]][1]
  return (name)
}

#read data with
data <- fread("weather_data_2000_2014.csv")
setnames(x=data,
         old=names(data),
         new=sapply(names(data), FUN=clean_names, USE.NAMES=F))

#filter for 4th moth
data.feb <- data[month==2 & temp>10,temp]
hist(data.feb)
mean(data.feb)
sd(data.feb)
plot(hist(data.feb), freq=FALSE, xlim=c(25,60), 
     ylim=c(0,0.1), xlab="T", ylab="density")
par(new=TRUE)
rm(data.feb)

#excercise R
feb.temp.2007 <- data[month==2 & year==2007, temp]
feb.temp.2008 <- data[month==2 & year==2008, temp]
feb.hum.2007 <- data[month==2 & year==2007, rhum]
temp.feb <-  data[month==2, temp]
# Plot qqplot
qqplot(x = feb.temp.2007, 
       y = feb.hum.2007, 
       plot.it = T)
abline(0, 1, col = "red")

# QQNORM tests QQPLOT tests are made to make sure that
# both elements are different
pnorm(40, mean(temp), sd(temp))/ pnorm(45, mean(temp), sd(temp))

boxplot(temp.feb)

require(outliers)
# Finds value with largest difference between 
# it and sample mean, which can be an outlier.
outlier(temp.feb)
#grubbs
grubbs.test(temp.feb, type=20)



require(extremevalues)
ol <- getOutliers(temp.feb)
ol
summary(ol)
outlierPlot(sample(temp.feb,10000), ol)






original.correct = 0
change.correct = 0
total = 100000
for (i in 1:total){
  car <- sample(1:3, 1)
  original_choice <- sample(1:3, 1)
  choice = original_choice
  
  if (car != original_choice){
    while (original_choice == choice){
      choice = sample(1:3, 1)
    }
  }
  if (car == choice){
    change.correct = change.correct + 1
  }
  if (car == original_choice){
    original.correct =original.correct + 1
  }
}
original.correct/total
change.correct/total