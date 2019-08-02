##############################################################
#
# Homework: linear regression with Kuipers 2008 used car data 
#
##############################################################

#@ 1
# Set the working directory to be whatever directory you want to use.
# This directory should contain the files "lin-regr-util.R" and
# "kuiper-2008-cars.csv" that you downloaded from iLearn.
#
# Also, "source" the file "lin-regr-util.R"

# YOUR CODE HERE

mydir="C:/Users/matth/Documents/CST 383/R programs/hw5"
setwd(mydir)

source("lin-regr-util.R")

#
# read the data
#

#@ 1
# Download the file "kuiper-2008-cars.csv" from iLearn.
# Write R code to read the data as a data frame and assign
# it to variable cars

# YOUR CODE HERE
cars = read.csv(file="kuiper-2008-cars.csv")

#
# preprocess data
#

# notes:
#  - mileage means total number of miles
#  - R treats factors as indicator variables, automatically

# turn Doors into an indicator variable
cars$FourDoor = as.numeric(cars$Doors == 4)
cars$Doors = NULL

# turn Cylinder into an indicator variable
cars$cyl6 = as.numeric(cars$Cylinder == 6)
cars$cyl8 = as.numeric(cars$Cylinder == 8)
cars$Cylinder = NULL

# turn Type into indicator variable (use all but one to avoid correlation)
cars$Convertible = as.numeric(cars$Type == "Convertible")
cars$Coupe = as.numeric(cars$Type == "Coupe")
cars$Hatchback = as.numeric(cars$Type == "Hatchback")
cars$Sedan = as.numeric(cars$Type == "Sedan")
cars$Type = NULL

# don't need make as it is implied by Model, but put
# Make at the front of Model
cars$Model = paste(cars$Make, cars$Model)
cars$Make = NULL

#@ 2
# produce a grid of scatterplots for the cars data, but using only 
# Price, Mileage, and Liter

plot(cars[,c("Price", "Mileage", "Liter")])

#@ 3
# using lm, perform simple linear regression to create a model
# that estimates Price using feature Mileage.  Assign your
# model to variable 'fit'.

# YOUR CODE HERE

fit = lm(cars$Price ~ cars$Mileage, data = cars)

#@ 4
# produce a scatter plot of Price (y axis) by Mileage (x axis), and
# superimpose the linear model you created as a dotted line on the
# scatterplot

# YOUR CODE HERE
plot(cars$Mileage, cars$Price, xlab = "Mileage", ylab = "Price", main = "Price over Mileage")
abline(fit, lty = 3, col = "blue")

# the summary of the fit shows high error
summary(fit)

# plots of the fit show that the distribution of residuals is
# not too far from a normal distribution
plot(fit)


#@ 5
# Using lm, create a linear model that estimates Price using 
# features Mileage, Cruise, and Leather.  
# Assign your model to variable 'fit2'.

# YOUR CODE HERE

fit2 = lm(cars$Price ~ (cars$Mileage + cars$Cruise + cars$Leather), data = cars)

# R-squared statistic is not good, but better
summary(fit2)

#@ 6
# produce a scatter plot in which you plot the actual
# used car prices (y axis) against the predicted used
# car prices.  Plot a dotted line showing where the
# points would be if the predicted values equalled the
# actual values.  Give your plot an appropriate title.

# YOUR CODE HERE

plot(fit2$fitted.values, cars$Price, data = cars, xlab = "Predicted Price", 
     ylab = "Actual Price", main = "Actual over Predicted Price")
abline(a = 0, b = 1, lty = 3, col = "blue")

#@ 7
# produce a similar scatter plot, but this time use
# function 'plot_predict_actual' that is defined in 
# file 'lin-regr-util.R'.  Use 2000 for the error band
# parameter, and use an appropriate title.

# YOUR CODE HERE

plot_predict_actual(fit2$fitted.values, cars$Price, 2000, "Actual over Predicted Price")

# the Residuals vs Fitted plot show errors getting bigger with higher price
plot(fit2)


