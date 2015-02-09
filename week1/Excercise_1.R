#loading data
library(RCurl)
library(data.table)
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
data <- getURL(url)
data <- read.csv(text = data)
names(data) <- c("sex","length","diameter","height","whole_weight",
                 "shucked_weight","visera_weight","shell_weight","rings")

#trying with datatable
data.dt <- data.table(data)

#get summary
summary(data)

#sorting 
data <- data[with(data, order(sex,shell_weight,rings)),]
data.dt <- data.dt[order(sex, shell_weight, rings),]
head(data)
head(data.dt)

#hist
hist(data$shucked_weight, freq=F)

y = hist(data$shucked_weight, freq=F)
sum(y$density) * (y$breaks[2] - y$breaks[1])

#weather data
w.data <- read.csv(file = )
