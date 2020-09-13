#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                 Title  : predict housing price    
#                 Author : Sasken
#                 Date   : 10-Jan-2017
#                
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = (ls(all=TRUE)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           Import the libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(caret)
library(corrgram)
library(randomForest)
library(mice)
library(dummies)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           Import the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

housingdf <- read.csv("D:/dat1/housingData.csv",sep=",")
str(housingdf)
dim(housingdf)
head(housingdf)
summary(housingdf)
attach(housingdf)
# detach(housingdf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#            EDA 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hist(price)
hist(size)
hist(uds)
hist(park)
par(mfrow=c(2,2))
plotInd <- names(housingdf)
head(housingdf)

# for all the variables(features) plot the boxplot 
 
for ( i in 1:length(plotInd)) {
  x <- boxplot(housingdf[,plotInd[i]], main= plotInd[i],horizontal = TRUE)
  outl <- x$out
  ind <- which(housingdf[,plotInd[i]] %in% outl)
  print(outl)
}

# x$out will hold the observations related to outliers

# for all the variables(features) plot the Histogram

for(i in 1:length(plotInd)) # Plot the boxplots for  all the features  
                           # and find out which ones need outlier treatment.
{
  hist(housingdf[,plotInd[i]])
  
}

# Reset the par to normal
dev.off()


# list_log<-names(housingdf)[c(3)]

# Treat the outliers 
fix.outlier <- function(df, feat){
  if(is.numeric(df[,feat])){
    per95 <- quantile(df[,feat],0.95)
    ifelse(df[,feat]> per95,per95,df[,feat])
  }
}


# check for the correlation 
library(corrplot)
housing.cor = cor(housingdf)
corrplot(housing.cor)

library(corrgram)
corrgram(housingdf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Create train and test split
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ind <- createDataPartition(housingdf$price, p=0.6,list = FALSE)
train <- housingdf[ind,]
dim(train)
head(train)
test <- housingdf[-ind,]
dim(test)
head(test)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   fit the linear reg model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
housing.fit <- lm(price~., data=train)
summary(housing.fit)

# What does the coefficient for the size suggest ?
# size variable suggests that the average effect of 1 sqfeet  
# is an increase of .045 (4,500 $)in price


test$pred <- predict(housing.fit, newdata = test)
# conf int
confint(housing.fit)

housing.fit1 <- lm(price~size+uds , data=train)
summary(housing.fit1)
# conf int
confint(housing.fit1)

train$pred1 <- predict(housing.fit1, newdata = train)

test$pred <- predict(housing.fit1, newdata = test)
length(test$pred)
SSE = sum(test$pred - test$price)^2
sqrt(sum(test$pred - test$price)^2)

housing.fit2 <- lm(price~size+uds+park, data=train)
summary(housing.fit2)

length(housing.fit2$fitted.values)
dim(train)

plot(housing.fit2$fitted.values, train$Price, xlab = "predicted", ylab = "Actual"
     , main = "Predicted Vs Actual")

plot(train$pred,residuals,abline(0,0))


fit2Pred <- predict(housing.fit2,newdata = test)

plot(fit2Pred, test$Price, xlab = "predicted", ylab = "Actual"
     , main = "Predicted Vs Actual")


# Are tere a relationship between the predictors and the
# response ?
# Ho: Bi=0  for every i 
# p value is corresponding to the F statistic 2.2e-16 ,
# this indicates clear evidence of a relationship between
# price and the other predictors


##Finding Residuals
residuals<-resid(housing.fit)
plot(residuals)
length(residuals)

##Plotting Residuals vs Predicted Values
##Checking Heteroskedastcity
##There should be no trend between predicted values and residual values
plot(train$pred,residuals,abline(0,0))


# write.csv(test,"D:/dat1/rahul.csv",row.names = FALSE)

# View(test)

# plot the predicted and actual data 

plot(test$price,col="blue",type="l")
lines(test$pred1,col="red",type="l")

# Comparing the models to choose the best one
anova(housing.fit,housing.fit1)
# null hypothesis states that the RSS between the
# rst and second model is not statistically significantly 
# different.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Validating the Assumptions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
# ~~~~~~~~~~~~~~~~~~~~~Check for Linearity
crPlots(housing.fit)

# ~~~~~~~~~~~~~~~~~~~~~Normality of the residuals
qqPlot(housing.fit)


# ~~~~~~~~~~~~~~~~~~~~~check for Homoscadasticity
library(lmtest)
bptest(housing.fit)
bptest(housing.fit1)

# ~~~~~~~~~~~~~~~~~~~~~Check for independence of errors
library(lmtest)
dwtest(housing.fit)
#if the p value is <.05 then the residuals are significantly
#correlated. In our case >.05 hence no evidence of autocorrelation

# ~~~~~~~~~~~~~~~~~~~~~Check for multicollinearity
# if the vif value is very high , greater than 5
# it represents collinearity
library(car)
vif(housing.fit)


