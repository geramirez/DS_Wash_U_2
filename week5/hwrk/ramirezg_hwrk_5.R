# set working dir
setwd("/home/gabrielramirez/Code/UW_Data_Science//DS_Wash_U_2//week5//hwrk")

# read data
data <- read.csv("day.csv", stringsAsFactors=FALSE)

# 1. Transform the "dteday" variable into a date/time object using the strptime function, 
# and create a new feature in the dataset that represents the day of the week. 
# The function strptime produces a result of type POSIXlt. It is a list, and one of its 
# elements is named "wday": the day of the week in numeric form. (You'll notice that 
# there is already a column in the dataset called "weekday". This exercise is for you 
# to practice manipulating dates in R. Your solution should correspond to this column.)

# Transform into POSIXct
data$dteday <- strptime(x = data$dteday, format = "%Y-%m-%d")
data$wday <- weekdays(data$dteday)
data$workingday

# 2.  Gather the total number of riders in each category and for each day of the week 
# into a contingency table. Are rider category and day of week independent (use a hypothesis test)?

con.table <- aggregate(x = data[,c("registered", "casual")], 
          by=list(data$wday), 
          FUN=sum, na.rm=TRUE)

chisq.test(con.table[,c('registered', 'casual')])
# With a p-value of 2.2e-16 we reject the null hypothesis that the 
# rider category and day of week are independent 


# 3. Consider the distribution of registered user rides for each day of the week. Is it the 
# same as the distribution of casual user rides for each day of the week (use a hypothesis test)?
ks.test(con.table$registered, con.table$casual)

# With a p-value of less than .05 we reject the null hypothesis that two datasets come 
# from the same distrubtion,. 

# 4. On average, do more people ride on the weekends or on weekdays (use a hypothesis test)? 
# This refers to the total number of rides per day, registered and casual.

# Making data nicer to look at (would not do if dataset was very large)
data$workingday <- with(data, ifelse(test = workingday == 1, yes = "weekday", no = "weekend"))
data$both <- with(data = data, expr = casual + registered)
con.table.workingday <- aggregate(x = data$both, 
                       by=list(data$workingday), 
                       FUN=mean, na.rm=TRUE)

# Chi-sqared test
rand <- sum(con.table.workingday$x)
con.table.workingday <- cbind(con.table.workingday, rand/2)
chisq.test(con.table.workingday[,2:3])

# With a p-value of 2.2e-16 we reject the null hypothsis that the total number of riders 
# remains constant between weekdays and weekend

# 5. Is it reasonable to apply a t-test in answering question 4? 
t.test(x = data[data$workingday == "weekend" ,"both"],
       y = data[data$workingday == "weekday" ,"both"])