##Claire Setser (cas3hp)
##Natalie Zimmer (naz6hd)
##Mike Wetklow (mw8hu)
##Stat 6021
##Project 1
##Due March 23, 2020

## set working directory
setwd("C:/Users/clair/Documents/STAT 6021/Project 1")

library(faraway)

##read data file in
read.table("mileage.txt", header=TRUE ,sep="")

## store data file with the variable name data
data<-read.table("mileage.txt", header=TRUE ,sep="")
attach(data)

##categorical variable x11
is.numeric(x11)
is.factor(x11) 
levels(x11)

##Know your goal, know your research question:  Client wants to find model that will help relate gas mileage to
## predictors X1 to X11.  Main concern is 1) Model Fits and 2) is simple

##intercept only model
regnull <- lm(y~1, data=data)
##model with all predictors
regfull <- lm(y~., data=data)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

####------------------------First Model-----------------------#######
##initial model considered from stepwise regression
result.stepwise<-lm(y~x1+x6) 
summary(result.stepwise)
anova(result.stepwise)
vif(result.stepwise)

##run partial F test compared to full model with all regressors
result.full<-lm(y~x1+x6+x2+x3+x4+x5+x7+x8+x9+x10+x11)
anova(result.stepwise,result.full) 

###---------------------Second Model--------------------------####
result.second<-lm(y~x1)
summary(result.second)
anova(result.second)

##run partial F test compared to full model with all regressors
result.full2<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)
anova(result.second,result.full2)
##decided not to use this one

###-----------------------Third Model-----------------------###
##model from backward elimination
result.backward<-lm(y~x5+x8+x10)
summary(result.backward)
anova(result.backward)
vif(result.backward)

##run partial F test compared to full model with all regressors
result.full3<-lm(y~x5+x8+x10+x1+x2+x3+x4+x6+x7+x9+x11)
anova(result.backward,result.full3)
##highest F-statistic in partial f-test

##Perform all possible regressions
library(leaps)
allreg <- regsubsets(y ~., data=data, nbest=11)

best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

##sort by various criteria
best[order(best$r2,decreasing=TRUE),]
best[order(best$adjr2,decreasing=TRUE),]
best[order(best$mse,decreasing=FALSE),] #(want smallest mse)
best[order(best$cp,decreasing=FALSE),]  #(want smallest cp)
best[order(best$bic,decreasing=FALSE),]

##----PRESS Statistic on chosen model----##
##PRESS statistic used in model validation as well as a criteria for model selection
#function that computes PRESS statistic for a regression model
PRESS <- function(linear.model){
  #1.Calculate predictive residuals
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  #2.Calculate the PRESS
  PRESS <- sum(pr^2)
  return(PRESS)}

##calculate PRESS statistic for reduced regression model
PRESS(result.backward)

##Can compare to PRESS statistic of other models considerd
##shows that chosen model has the smallest PRESS statistic
PRESS(result.stepwise)
PRESS(result.second)

##Calculate R^2 prediction for this model
sst <- (437.81+324.98+245.91+228.85)
1-(PRESS(result.backward)/sst)

####-----------------Regression Assumptions Check---------------####
##scatterplot matrix
pairs(~y+x5+x8+x10, lower.panel = NULL, main="Scatterplot Matrix")

##residual plot
plot(result.backward$fitted.values,result.backward$residuals,main="Residual plot")
abline(h=0,col="red")

##acf plot of residuals
acf(result.backward$residuals)

##QQ plot of residuals
qqnorm(result.backward$residuals)
qqline(result.backward$residuals, col="red")

##Create a Box Cox Plot
library(MASS)
boxcox(result.backward)
boxcox(result.backward, lambda = seq(-4, 4, 1/10)) 

##boxcox plot does not include 1 in the limits
##Apply log transformation on response y
y.log<-log(y)
result.log<-lm(y.log~x5+x8+x10)
summary(result.log) 

##look at assumptions again
##scatterplot matrix
pairs(~log(y)+x5+x8+x10, lower.panel = NULL, main="Scatterplot Matrix")

##residual plot
plot(result.log$fitted.values,result.log$residuals,main="Residual plot")
abline(h=0,col="red")

##acf plot of residuals
acf(result.log$residuals)

##QQ plot of residuals
qqnorm(result.log$residuals)
qqline(result.log$residuals, col="red")

##Create a Box Cox Plot
boxcox(result.log)
boxcox(result.log, lambda = seq(-4, 4, 1/10)) 