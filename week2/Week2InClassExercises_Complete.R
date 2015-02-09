
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



# Filter the data to get the February temperature data for the years in the data

data = data[month == 2,] 

# Further filter the data to remove values less than 10 degrees

data = data[temp > 9.99,]

# Use hist to create a histogram of the data, and estimate the probability the temperature is less than 40 degrees
# and estimate the median temperature

y = hist(data[,temp], freq = FALSE, bins =30)

# Use "mean" and "sd" to compute the mean and standard deviation of the temperature data

mn_temp = mean(data[, temp])
sd_temp = sd(data[, temp])

# Plot the density function of a normal distribution with the mean and standard deviation just computed
# and superimpose it with the histogram of the data

x_vals=seq(25,60,by = 0.1)
plot(hist(data[, temp]),freq=FALSE,xlim=c(25,60),ylim=c(0,0.1), xlab="T",ylab="density")
par(new=TRUE)
plot(x_vals, sapply(x_vals, function(x) dnorm(x,mn_temp,sd_temp,log = FALSE)))

# Use the same normal distribution to find the probability that the temperature is less than 40 degrees

pnorm(40, mn_temp, sd_temp, lower.tail=TRUE, log.p = FALSE)

# Use the same normal distribution to find the median temperature

qnorm(.5, mn_temp, sd_temp, lower.tail = TRUE, log.p = FALSE)


----
#EXERCISE: Weather Data and QQ Plots
  
# Get temperature data from February 2007 and February 2008
  
weather.data.feb = weather.data[weather.data$Month == 2,]
temp.data.feb2007 = weather.data.feb$Temp..F.[weather.data.feb$Year == 2007]
temp.data.feb2008 = weather.data.feb$Temp..F.[weather.data.feb$Year == 2008]
  
# Draw a QQ plot with the two samples.  Use the command "abline" to add the line x=y to the 
# plot as a visual aid.  What can you tell about the temperatures in the 2 years?

qqplot(temp.data.feb2007,temp.data.feb2008, plot.it = TRUE)
abline(0,1, col="red")

# Get temperature and humidity data from February 2007

hum.data.feb2007 = weather.data.feb$RHum....[weather.data.feb$Year == 2007]

# Draw a QQ plot with the two samples.  Use the command "abline" to add the line x=y to the 
# plot as a visual aid.  What can you tell about the relationship between temperature and humidity?
  
qqplot(temp.data.feb2007,hum.data.feb2007, plot.it = TRUE)
abline(0,1, col="red")

---
  
# EXERCISE: Conditional Probability
  
# From the normal distribution for the temperature exercise, what is the probability that T<40 degrees
# given that you already know T<45 degrees?
  
pnorm(40,mn_temp,sd_temp)/pnorm(45,mn_temp,sd_temp)
  
---
# DEMO: Outliers and Imputation

#plot
#hist

# Don't forget boxplot from last week!
  
boxplot(weather.data.feb$Temp..F.)

#qqnorm
  
# packages for outliers
require(outliers)
outlier(weather.data$Temp..F.)
grubbs.test(weather.data$Temp..F.)

require(extremevalues)
ol <- getOutliers(weather.data$Temp..F.)
ol
summary(ol)
outlierPlot(weather.data$Temp..F., ol)

# artifically add some (1%) missing values to the humidity variable - as a function of temperature
weather.data.na = weather.data
n = nrow(weather.data.na)
weather.data.na$RHum....[sample(which(weather.data.na$Temp..F. > 70), n/100)] = NA

# find percent missing
sum(is.na(weather.data.na$RHum....))/length(weather.data.na$RHum....)

hist(which(is.na(weather.data.na$RHum....)))

temp_with_hum_observed = weather.data.na$Temp..F.[!is.na(weather.data.na$RHum....)]
temp_with_hum_missing = weather.data.na$Temp..F.[is.na(weather.data.na$RHum....)]

qqplot(temp_with_hum_missing, temp_with_hum_observed)
abline(0,1, col="red")

qqplot(temp_with_hum_missing, temp_with_hum_observed, ylim=c(50,90))
abline(0,1, col="red")

# look for runs of missing data
ind <- sapply(weather.data.na$RHum...., function(x) if(is.na(x)){1}else{0})
runs <- rle(ind)
runs
run.df <- data.frame(runs$lengths, runs$values)
run.df = run.df[with(run.df, order(-runs.values, -runs.lengths)),]
run.df[1:10,]

# DEMO: Multiple Imputation with Amelia
# is it normal?
hist(weather.data.select$RHum....)

# eyeball norm says: could be worse, let's press on with Amelia for imputation
require(Amelia)
# drop the empty column for pressure
weather.data.na = weather.data.na[,-which(names(weather.data.na)=="Pres.mbar.")]
amelia.results = amelia(weather.data.na, idvars=c("Year", "Month", "Day", "Time"))

# Show the distribution of the observed and imputed values
plot(amelia.results)
# Recall the relationship between missing values and temperature
plot(weather.data.na$Temp..F. ~ weather.data.na$RHum....)

# Compare means before and after imputation 
mean(weather.data.na$RHum....)
mean(weather.data.na$RHum...., na.rm=TRUE)
for(i in 1:5) {
  print(mean(amelia.results$imputations[[i]]$RHum....))
}

# Compare distributions before and after imputation
hist(weather.data.na$RHum...., xlim=c(0,100))
hist(amelia.results$imputations[[4]]$RHum...., xlim=c(0,100))
qqplot(weather.data.select$RHum...., amelia.results$imputations[[1]]$RHum....)
abline(0,1,col="red")

