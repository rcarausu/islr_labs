# Chapter 4 lab (page 169)
# Install ISLR Package from the Packages Tab

# Using the Smarket data which is part of the ISLR library

library(ISLR)
# getting the names of the variables
names(Smarket)
# dimensions of the library (observations X n variables)
dim(Smarket)

summary(Smarket)

# matrix of scaterplots
pairs(Smarket)

# matrix with all the pairwise correlations among the predictors in the data set
# This helps us identify which variables affect the most our predictor (Today's return) so we can use less variables
# when we want to classify. Between -1 and 1. Negative numbers indicate low correlation
# In our example correlation is between Year-Today and Volume-Today.
cor(Smarket [, -9]) #To avoid error because Direction variable is qualitative

# We attach the data so we can call the variables by their name instead of Smarket$Year
attach(Smarket)
plot(Volume)

# LOGISTIC REGRESSION
# We fit a regression model using the glm() function (generalized linear models) that includes logistic regression
# in order to predic Direction using Lag1 through Lag5 and Volume. family=binomial indicates it is logistic regression

# The smallest p-value is for Lag1 (0.145). The negative coefficient for this predictor suggest that if the market had a 
# positive return yesterday, then it is less likely to go up today. However a value of 0.145 is still a large p-value and
# there is no clear evidence of a real association between Lag1 and Direction
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

# We use the coef() function in order to access just the coefficients for this fitted model
coef(glm.fit)
# We can also use the summary() function to access particular aspects of the fitted model, such as the p-values for the coefficients
summary(glm.fit)$coef
summary(glm.fit)$coef[,4] # accessing just p values

# The predict() function can be used to predict the probability that the market will go up, given values of the predictors
# type=response option tells R to output the probabilities of the form P(Y= 1/X), as oposed to other information as the logit.
glm.probs = predict(glm.fit, type="response")
# These probabilities correspond to the market going up. We can know this with the contrasts() function for Direction
glm.probs[1:10]

contrasts(Direction) # 1: Up, 0: down

# To make a prediction whether the market will go uo or down on a particular day, we must convert these predicted probabilities
# into class labels (Up or Down)
# The following commands create a vector of class predictions based on whether the predicted probability of a market increase is 
# greater than or less than 0.5
glm.pred=rep("Down", 1250) # Creates a vector of 1250 Down elements
glm.pred[glm.probs > .5]="Up" # Sets to Up all elements for which the predicted probability of market increase exceeds 0.5

# We use the table() function to create a confussion matrix to determine how many observatons were correctly or incorrectly classified
# Diagonl elements of the confusion matrix indicate correct predictions, while the off-diagonals represent incorrect predictions
table(glm.pred, Direction)
# (507 Correcct Up, 145 Correct Downs)
(507+145)/1250
# The mean() function can be used to compute the fraction fo days for which the prediction was correct, in this case 52.2%
mean(glm.pred==Direction)
# This result is misleading because we trained and tested the model of the same sate of 1250 observations, so the
# TRAINING ERROR Rate is 100-52.2 = 47.8%
# To properly test the model, we need to use part of the data as training and the other part as test, the we calculate the test data

train=(Year<2005) # We hold data from 2005 for test
Smarket.2005=Smarket[!train,] # submatrix of the stock market data containing only the observations for which train is FALSE
dim(Smarket.2005)
Direction.2005=Direction[!train]
# Object train is a vector of 1250 elements. Elements of observations before 2005 are set to TRUE, and after to FALSE.

# Now we do logistic regression only for the train data using the subset argument
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit, Smarket.2005, type="response")
# We have trained and tested our model on 2 separate datasets, for <2005 and >=2005

glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="UP"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005) # 0.31

# 44/(34+44)
mean(glm.pred!=Direction.2005) # 0.69 Test error is higher than random guessing so it is not a good result (it differs from the book, 0.52)

# Since the error is so high, we use just Lag1 and Lag2 since the p-value seems to have the higher predictive power

glm.fit=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit, Smarket.2005, type="response")
glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="UP"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
# 106/(106+76)
mean(glm.pred!=Direction.2005)

# Suppose that we want to predict the returns associated with particular
# values of Lag1 and Lag2. In particular, we want to predict Direction on a
# day when Lag1 and Lag2 equal 1.2 and 1.1, respectively, and on a day when
# they equal 1.5 and −0.8. We do this using the predict() function.
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")


# LINEAR DISCRIMINANT ANALYSIS (Page 176)

