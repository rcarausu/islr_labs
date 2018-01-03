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

# We perform LDA using the lda() function, part of the MASS libreary
# Usage is similar to glm() functon, without the family argument
# We use only the data before 2005
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=train)

"lda.fit
Call:
  lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

Prior probabilities of groups:
  Down       Up 
0.491984 0.508016 

Group means:
  Lag1        Lag2
Down  0.04279022  0.03389409
Up   -0.03954635 -0.03132544

Coefficients of linear discriminants:
  LD1
Lag1 -0.6420190
Lag2 -0.5135293
"
# The plot() function produces plots of the linear discriminants, obtained for computing -0.642*Lag1-0.514*Lag2 for each of the training
# observations
plot(lda.fit)

# The predict function returns a list of3 elements: 
# First element (class) contains LDA's predictions about the movement of the market
# Second element (posterior) is a matrix whose kth column contains the posterior probability that the corresponding observation
# belongs to the klth class
# Third element (x) contains the linear discriminants
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005) # The LDA and logistic regression predictions are almost identical

# Applying a 50% threshold to the posterior probabilities allows us to recreate the predictions contained in lda.pred$class
sum(lda.pred$posterior[,1]>=.5)
# 70
sum(lda.pred$posterior[,1]<.5)
# 182

# Notice that the posterior probability output bt the model corresponds to the probability that the market will decrease
lda.pred$posterior[1:20,1]
lda.class[1:20]

# We can easily change the threshold if we want, for example, to predict a market decrease only if we are very certain that the market
# will indeed decrease on that day -say, if the posterior probability is at least 90%
sum(lda.pred$posterior[,1]>.9)

# QUADRATIC DISCRIMINANT ANALYSIS
library(MASS)
# We use the qda() function pasrt of the MASS library
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)

"qda.fit
Call:
qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

Prior probabilities of groups:
    Down       Up 
0.491984 0.508016 

Group means:
            Lag1        Lag2
Down  0.04279022  0.03389409
Up   -0.03954635 -0.03132544"

qda.class=predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)
# 0.60 This is a pretty high value especially for stock market data, and fitting without using the 2005 data, so we can say
# the quatratic for assumed by QDA may capture the true relationship more acurately than the linear forms of logistic regression and LDA.


# K-NEAREST NEIGHBOURS

# We use the knn() function part of the class library
# This function forms prediction using a single command with 4 inputs:
# 1 - a matrix containing the predictors associated with the training data, labeled train.X below
# 2 - a matrix containing the predictors associated with the data for which we wish to make predictions, labeled test.x below
# 3 - a vector containing the class labels for the training observations, labeled train.Direction below
# 4 - a value for k, the number of nearest neighbours to be used by the classifier

# We use cbind() function (column bind) to bind Lag1 and Lag2 into two matrices, one for the training set and the other for the test set
library(class)
train.X=cbind(Lag1, Lag2)[train,]
test.X=cbind(Lag1, Lag2)[!train,]
train.Direction=Direction[train]

# Now the knn() function can be used
# We set a random seed before because if several observations are tied as nearest neighbours, then R will randomly break the tie,
# Therefore a seed must be set in order to ensure reproducibility of results
set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
(83+43)/252
# 0.5 Only 50% of observations are correctly predicted, it may be because k=1 is to flexible, we try with k=3
knn.pred=knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)
# 0.536 We get a slight improvement

# CONCLUSION: It seems that QDA has the best results so far













