# THE VALIDATION APPROACH
library(ISLR)
set.seed(1) # It's a good idea to set a seed to be able to reproduce CV results in the future
train=sample(392, 196) # random subset of 196 observations out of 392 original ovservations

lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)       
mean((mpg-predict(lm.fit, Auto))[-train]^2) # calculating the mse of all the observations out of the train set
# 26.14

# MSE for quadratic and cubic regressions
lm.fit2=lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
# 19.82
lm.fit3=lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)
# 19.78

# LOOCV
# We use the glm() and cv.glm() functions. glm without family argument does a linear regression
glm.fit=glm(mpg~horsepower, data=Auto)
coef(glm.fit)

library(boot) # cv.glm() is part of the boot library
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto, glm.fit)
cv.err$delta
"cv.err$delta
[1] 24.23151 24.23114" # This is the LOOCV statisctic calculated with the MSE CV(n)=1/n*Sum(MSEi)
# The CV estimate for the test error is approximately 24.23
# We repeat this procedure for increasingly complex polynomial fits using the for() function which iteratively fits the polynomial 
# regressions for polynomials of orders i=1 to i=5, computes the associated CV error and stores it in the ith element of the vector cv.error
cv.error=rep(0,5)
for(i in 1:5){
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
#[1] 24.23151 19.24821 19.33498 19.42443 19.03321

# We see a drop in error between linear and quadratic fits, but afterwards the improvement is not noticeable

# K-FOLD CROSS-VALIDATION
# We can also use cv.glm() to implement k-fold CV
# Below we use k=10
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit, K=10)$delta[1]
}
cv.error.10
# [1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609 19.71201 18.95140 19.50196
# We notice a shorter computation time than with LOOCV
# Still little evidence that using cubic or higher-order polynomial terms leads to lower test error than simply using a quadratic fit






