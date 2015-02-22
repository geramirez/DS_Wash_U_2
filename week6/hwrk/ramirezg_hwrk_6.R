setwd("Code/UW_Data_Science/DS_Wash_U_2/week6/hwrk/")

data <- read.csv("abalone.data.csv")
names(data) <- c("sex", "length", "diameter", "height",
                "whole_weight", "shucked_weight", "viscera_weight",
                "shell_weight", "rings")

# 1. Plot the number of rings as a function of length.
with(data = data, plot(rings ~ length))

# 2. Fit a linear model to this data (rings  = a*length + b) 
# using R’s lm command. Examine the output of the summary 
# table for the fit. Is length a significant factor?

fit.1 <- lm(formula = rings ~ length, data = data)
summary(fit.1)

# Yes length is a significant factor 

# 3. There are three sexes of abalone: male, female, and 
# immature. Filter the data so that only the immature abalone 
# remain. Fit the same model to this data (rings = a*length + b).
# Examine the output of summary: is this model a better or 
# worse than the model fit to all of the data?

fit.2 <- lm(formula = rings ~ length, data = data[data$sex == "I", ])
summary(fit.2)

# This model is better than the model fit to all the data.


# 4. Still working with the immature abalone only, add Height 
# and Diameter to the model (rings = a*length + b*height + etc.). 
# Examine the output of summary: what are the significant 
# factors in this new model? Compare the result to the
# “length only” model and explain why the two results on 
# consistent with each other.

fit.3 <- lm(formula = rings ~ length + height + diameter, data = data[data$sex == "I", ])
summary(fit.3)

# The R-squared of this new model is closer to 1 than the length only
# model. It seems that height and diameter were better predictors
# for the number of rights than length; hence, in this model
# the predictive power of length appears to be less. 


# 5. Still working with the immature abalone only, add all of
# the factors to the model (except Sex: since we only have 
# immature abalone, this value is the same for every data 
# point) (rings = a*length + b*height + etc.). Examine the
# residuals and summarize your observations. Use graphical
# methods (histogram, qqplot, etc.) and also plot the 
# residuals as a function of the number of rings.

# Create formula
f <- as.formula(paste("rings ~ ", paste(setdiff(names(data), c("sex", "rings")), collapse= " + ")))
fit.4 <- lm(formula = f, data = data[data$sex == "I", ])
summary(fit.4)

# hist of residuals
hist(fit.4$residuals)

#plot summaries
plot(fit.4)

with(data = data[data$sex == "I", ], plot(rings ~ fit.4$residuals))