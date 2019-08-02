##############################################################
#
# Homework: logistic regression with with heart data
#
##############################################################

#
# do logistic regression with heart data
#

#
# read the data
#https://raw.githubusercontent.com/grbruns/cst383/master/heart.dat
heart = read.table("https://raw.githubusercontent.com/grbruns/cst383/master/heart.dat", quote = "/")
names(heart) <- c("AGE", "SEX", "CHESTPAIN", "RESTBP", "CHOL",
                  "SUGAR", "ECG", "MAXHR", "ANGINA", "DEP", "EXERCISE", "FLUOR",
                  "THAL", "OUTPUT")
names(heart) = tolower(names(heart))
heart$output = heart$output - 1    # convert to 0-1 range

#
# do a little exploration
#
#For convenience I created a vector containing the numeric features
#(in other words, features that are "real-valued").
# these are the real-valued features:
rv = c("age", "restbp", "chol", "maxhr", "dep")

#The mean age of the human subjects in the data is about 55 years old.
hist(heart$age, main="age of test subjects", xlab="age", col="red4")



#From a grid of scatterplots of the numeric features, it appears that
#no pairs of features are strongly correlated.  Max heart rate and age
#appear somewhat correlated.  The feature 'dep' could
#perhaps be transformed, as the values are crowding the left side of
#the plot.
# good -- predictors not strongly correlated
plot(heart[,rv])

# another way of looking at the correlation
#The correlation matrix backs up the idea that the numerical features aren't
#strongly correlated.

heatmap(cor(heart[,rv]))

#Higher resting blood pressure and lower maximum heart rate appear to
#be associated with heart disease.
#plot(output ~ age, data=heart)

plot(restbp ~ maxhr, data=heart, col=c("green", "red")[heart$output+1], pch=16,
     main="heart disease by resting blood pressure and max heart rate")
legend("topleft", c("heart disease", "no heart disease"), fill=c("red", "green"), 
       inset=0.02)


### Building a logistic regression model

#First, we create training and test data sets.  We have to use
#some care here, as our data set is small.

# Note that I'm using the same 'lin-regr-util.R' file that we used with linear regression
source("https://raw.githubusercontent.com/grbruns/cst383/master/lin-regr-util.R")

set.seed(123)

splits = split_data(heart, frac=c(3,1))
tr_dat = splits[[1]]
te_dat = splits[[2]]

#We build a first model and look at the model
# summary to see which features appear to be relevant.
# Using glm, create a  model that uses 
# features chol , maxhr , dep and chestpain.  
# Assign your model to variable 'fit1'. Then get model summary. 

# YOUR CODE HERE
fit1 = glm(output ~ chol + maxhr + dep + chestpain, data = tr_dat, family = binomial)
summary(fit1)
# The summary indicates the 'chol' feature isn't relevant 
# in the presence of the other predictors of our model.

### Classifying test data

# We run our model on the test data, and classify examples in the test
# data using a threshold of 0.5.

#  The summary output from our fit only gives a rough idea of the
# usefulness of our model.  For a better idea we should use the
# model as a classifier and see how well it predicts heart disease
# on the test data.

# A prediction from a logistic model is an estimate of the probability
# of heart disease.  To turn the logistic model into a classifier we can
# pick a threshold and predict heart disease if the probability exceeds
# the threshold.  By default we can use 0.5 as our threshold.
  
y = predict(fit1, newdata=te_dat, type="response")
predicts = as.numeric(y > 0.5)
actuals = te_dat$output
conf_mtx = table(predicts, actuals)
conf_mtx


# The accuracy of our classifier is not bad.

# The "accuracy", or success rate, of our model, is the fraction of
# test examples that are successfully classified.
  

succ_rate = mean(predicts == actuals)
round(succ_rate, 3)

### Assessing the model

# Looking at the output of the model on test cases where heart
# disease is present, and on test cases where heart disease is
# not present, we can see that the model is not working
# especially well.  We can also see that a classification threshold
# of about 0.5 is probably about right.

par(mfrow=c(1,2))
hist(y[actuals == 0], main="Output when no heart disease", 
     breaks=10, xlim=c(0,1), ylim=c(0,15), col="red4", xlab="model predictions")
hist(y[actuals == 1], main="Output when heart disease", 
     breaks=10, xlim=c(0,1), ylim=c(0,15), col="red4", xlab="model predictions")

# Here is the same information, presented as a double density plot.

# Hint: remember that you use something like plot(density(y[actuals == 1])) to
# create a density plot, and that you use 'lines' to add lines to an existing plot. </i>
  

plot(density(y[actuals == 1]), col="blue", xlim=c(0,1), ylim=c(0, 3), 
     main="double density plot", xlab="logistic regression output", lwd=2)
lines(density(y[actuals == 0]), col="green4", lwd=2)
abline(v=0.50, lty=2)
legend("topright", c("heart disease", "no heart disease"), fill=c("blue", "green4"), inset=0.03)


# To see the impact of changing the threshold level, we calculate
# model precision and recall at variout threshold levels.

# Precision shows how often a positive prediction is correct.
# Recall shows how often positive examples are classified as
# positive.  In the case of heart disease, high precision means
# people diagnosed with heart disease really have it.
# High recall means that people with heart disease are diagnosed
# with heart disease.

# In terms of medical diagnosis, the problem with low precision
# is that we tell someone they have heart disease but they don't
# really have it.  The problem with low recall is that people
# who have heart disease aren't being told they do.  The cost
# associated with low precision is unnecessary tests, drugs,
# and procedures.  The cost associated with low recall is that
# a patient is not treated and possibly has a heart attack or
# other serious problem.
  

prec_recall_summary = function(predicts, actuals) {
  thresh = seq(0, 1, length.out=50)
  prec_rec = data.frame()
  actuals = factor(as.numeric(actuals))
  for (th in thresh) {
    predicts = factor(as.numeric(y >= th), levels=c("0","1"))
    prec_rec = rbind(prec_rec, as.vector(table(predicts, actuals)))
  }
  names(prec_rec) = c("TN", "FP", "FN", "TP")
  prec_rec$threshold = thresh
  prec_rec$precision = prec_rec$TP/(prec_rec$TP + prec_rec$FP)
  prec_rec$recall    = prec_rec$TP/(prec_rec$TP + prec_rec$FN)
  prec_rec$false_pos = prec_rec$FP/(prec_rec$FP + prec_rec$TN)
  return(prec_rec)
}

prec_rec1 = prec_recall_summary(predicts, actuals)


# These plots show precision and recall by threshold value.  With
# the threshold at 0.5, about 75% of the people diagnosed with
# heart disease really have it, and about 80% of the people who
# have it are diagnosed to have it.


par(mfrow=c(2,1))
par(mar=c(4,4,2,2))
plot(precision ~ threshold, data=prec_rec1, type="l", ylim=c(0, 1), col="red4", lwd=1.5)
grid(col="grey70")
plot(recall ~ threshold, data=prec_rec1, type="l", ylim=c(0,1), col="red4", lwd=1.5)
grid(col="grey70")
par(mfrow=c(1,1))


# The ROC plot suggests that the classifier is working reasonably well.

# The receiver operating characteristic (ROC) plot gives an overall idea of how
# well the classifier is working.  The curve for a perfect classifier would
# hug the left and top edges of the plot.  The curve for a classifier that
# makes random decisions would be a diagonal line from the lower-left to
# the upper-right.  See p. 147 of our text (ISLR) for more information about ROC plots.</i>
  

plot(recall ~ false_pos, data=prec_rec1, type="l", lwd=2, col="red4", 
     main="receiver operating characteristic", xlab="false positive rate", ylab="true positive rate")
grid(col="grey70")

