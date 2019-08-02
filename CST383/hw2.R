#@ 1 
# assign the value of variable z to variable x
x = z
#@ 2 
# assign "hello" to variable msg
msg = "hello"
#@ 3 
# check whether variable msg is numeric
is.numeric(msg)
#@ 4 check whether variable b is logical
is.logical(b)
#@ 5 assign the result of squaring x to variable x2
x2 = x^2
#@ 6 
# compute the length of the string obtained by appending strings 
# s1 and s2, with a space between them
length(paste(s1, s2, sep = " "))
#@ 7 compute the substring consisting of the 2nd-4th characters of string s
substr(s, 2, 4)
#@ 8 assign the result of testing whether x is greater than 5 to variable xt
xt = x > 5
#@ 9 check if variable x is NA
is.na(x)
#@ 10
# Assign to variable x a vector of length 10 of
# randomly-generated numbers from 1 to 3.  (Hint: consider
# using function 'sample', and if you use it, think about
# parameter 'replace'.)
# (assignment to x)
x = sample(1:3, 10, replace = TRUE)
#@ 11
# We now go from dice to birthdays.  Write R code to compute a vector 'bdays' 
# containing the birthdays (as a number from 1 to 365) of 30 randomly-selected people.
# This is not too different from problem 1.
# (assignment to bdays)
bdays = sample(1:365, 30, replace = TRUE)
#@ 12
# Write an R expression that computes 0 if vector x contains no duplicates, and 
# computes 1 otherwise.
# (expression)
ifelse(length(x) > length(unique(x)), 1, 0)
#@ 13
# Write R code to compute the birthdays of 30
# randomly-selected people 1000 times, and assign
# to variable bday_prob the fraction of the time that 
# two people share the same birthday
# (assignment to bday_prob)
people = 30
repeats = 1000
days = matrix(c(1:365))
prob1 = data.frame()
prob.at.least.1 = data.frame()
prob2 = data.frame()
bday_prob = function(people, repeats) {
  for (i in 1:repeats){
    sampl = days[sample(days, people, replace = TRUE),]
    diff.birthday = length(unique(sampl)) < people
    prob1 = rbind(prob1,diff.birthday)}
  trues = sum(prob1$TRUE. == TRUE) + sum(prob1$FALSE. == TRUE)
  trials = repeats
  options(digits=3)
  prob.at.least.1 = trues / trials * 100
  prob.at.least.1 <<- data.frame(prob.at.least.1) 
}
#@ 14
# Write an R function that estimates the probability that
# two people among a room of randomly-selected people will
# share the same birthday.  The function should be named
# 'bday_prob', and should take a parameter 'num_people'.
# (assignment to bday_prob).  This function should be based
# on your code for the previous problem.
num_people = 30
repeats = 1000
days = matrix(c(1:365))
prob1 = data.frame()
prob.at.least.1 = data.frame()
prob2 = data.frame()
bday_prob = function(num_people, repeats) {
  for (i in 1:repeats){
    sampl = days[sample(days, people, replace = TRUE),]
    diff.birthday = length(unique(sampl)) < people
    prob1 = rbind(prob1,diff.birthday)}
  trues = sum(prob1$TRUE. == TRUE) + sum(prob1$FALSE. == TRUE)
  trials = repeats
  options(digits=3)
  prob.at.least.1 = trues / trials * 100
  prob.at.least.1 <<- data.frame(prob.at.least.1) 
}
#@ 15
# Test your function 'bday_prob' by calling it with input parameter values 10, 20, 30, 40, and 50
# (expression)
x = bday_prob(10, repeats)
x
x = bday_prob(20, repeats)
x
x = bday_prob(30, repeats)
x
x = bday_prob(40, repeats)
x
x = bday_prob(50, repeats)
x
#@ 16
# Create a plot that will show the estimated probability that
# two people in a room will share the same birthday.  The x axis
# of the plot should range from 5 to 50, and indicate the number
# of people in the room.  The y axis should range from 0 to 1 and
# represent the probability.  To create data for the plot, run
# your birthday function on all input values of 5 to 50.Improve your plot 
#by using color, by adding a title, x and y axis labels, and a grid.  
#Make the title "Esimated prob. of sharing a birthday",
# the x axis label "Number of people", and the y axis label "Prob. of sharing a birthday".
# Plot the curve as a line, not points.
# (plot)


