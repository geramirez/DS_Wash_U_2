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

#summarize data
summary(data)

hist(data[temp>0 & temp<130, temp])
