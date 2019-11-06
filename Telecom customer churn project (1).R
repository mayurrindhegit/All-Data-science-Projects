# This is how you call the packages
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)


# Reading data; just change the file path to fetch the data.
data.np <- read.csv("C:/Users/PankajR/Desktop/churn.csv",header = TRUE)
data <- data.np
head(data)
# Data sanity check
dim(data)
str(data)
summary(data)

## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))

## removing the rows with missing values
nrow(data)
data<- na.omit(data)## removes the missing values
data<- data[complete.cases(data),] ## removes the missing values; both are same
nrow(data)

## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))


# Converting necessary variables into factor
data$SeniorCitizen <- as.factor(data$SeniorCitizen)



# Training and Test data
set.seed(113)
partitionData <- function(dataset=NULL,prob,Target)
{
  inTrain <- createDataPartition(y = dataset[,Target],p = prob, list = FALSE)
  inTrain
}
train.sample <-partitionData(dataset=data,prob=0.7,Target="Churn")

data.train <- data[train.sample,]
data.test <- data[-train.sample,]

nrow(data.train)
nrow(data.test)


# Logistic Regression on full data

model <- glm(Churn~., data=data.train, family=binomial())
summary(model)

## Remove the insignificant variable
model <- glm(Churn~ gender +	SeniorCitizen + Partner + Dependents + tenure +	PhoneService + MultipleLines
             + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV 
             + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges 
             + TotalCharges, data=data.train, family=binomial())
summary(model)


##
model <- glm(Churn~ gender +	SeniorCitizen + Partner + Dependents + tenure +	PhoneService 
             + I(MultipleLines=="Yes")+ InternetService + I(OnlineSecurity=="Yes")+ I(OnlineBackup=="Yes") 
             + I(DeviceProtection=="Yes") + I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)

## Remove Partner, PhoneService, Dependents, gender 
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService + I(OnlineSecurity=="Yes")+ I(OnlineBackup=="Yes") 
             + I(DeviceProtection=="Yes") + I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)

## Remove  I(OnlineBackup=="Yes"), I(DeviceProtection=="Yes") , I(OnlineSecurity=="Yes")
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService   +  I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)

## Remove   I(TechSupport=="Yes")
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + I(PaymentMethod=="Credit card (automatic)")
             + I(PaymentMethod=="Electronic check")+MonthlyCharges +	TotalCharges , data=data.train, family=binomial())
summary(model)

## Remove   I(TechSupport=="Yes")
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
             + I(PaymentMethod=="Electronic check")+MonthlyCharges +	TotalCharges , data=data.train, family=binomial())
summary(model)

vif(model)

# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)



## Remove   MonthlyCharges, tenure
model <- glm(Churn~ 	SeniorCitizen  
             + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
             + I(PaymentMethod=="Electronic check")+	TotalCharges , data=data.train, family=binomial())
summary(model)

vif(model)

# Difference between -2LL of Null model and model with variables
modelChi <- model$null.deviance - model$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


# Hosmer and Lemeshow R square
R2.hl<-modelChi/model$null.deviance
R2.hl


# Cox and Snell R Square (the last number; 

R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data.train))
R.cs

# Max rescaled R square (Nagelkarke) 

R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(data.train))))))
R.n



######### Lackfit Deviance ######################################################
residuals(model) # deviance residuals
residuals(model, "pearson") # pearson residuals

sum(residuals(model, type = "pearson")^2)
deviance(model)

#########Large p value indicate good model fit
1-pchisq(deviance(model), df.residual(model))

#######################################################################################
#Function - HS Test

hosmerlem <- function (y, yhat, g = 10) {
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)),
                 include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## This is not a very useful test. Some authors have suggested that sometimes it produces wrong result##
## High p value incidates the model fits well

hosmerlem(y = as.integer(data.train$Churn), yhat = fitted(model))
################################################################################################################
# Hosmer and Lemeshow test in a different way

library(ResourceSelection)
hl <- hoslem.test(as.integer(data.train$Churn), fitted(model), g=10)
hl

#####################################################################################################################
# Coefficients (Odds)
model$coefficients
# Coefficients (Odds Ratio)
exp(model$coefficients)

# Predicted Probabilities
prediction <- predict(model,newdata = data.train,type="response")
prediction

#write.csv(prediction, file = "C:\\Users\\Subhojit\\Desktop\\Logistic Regression\\Prepared by me\\pred.csv")

library(pROC)
rocCurve   <- roc(response = data.train$Churn, predictor = prediction, 
                  levels = rev(levels(data.train$Churn)))
data.train$Churn <- as.factor(data.train$Churn)

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$Churn)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)

#########################################################################################################################
### KS statistics calculation
data.train$m1.yhat <- predict(model, data.train, type = "response")

library(ROCR)
m1.scores <- prediction(data.train$m1.yhat, data.train$Churn)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

############################################################################################################


###################### Residual Analysis ################################################################################


logistic_data <- data.test

logistic_data$predicted.probabilities<-fitted(model)
logistic_data$standardized.residuals<-rstandard(model)
logistic_data$studentized.residuals<-rstudent(model)
logistic_data$dfbeta<-dfbeta(model)
logistic_data$dffit<-dffits(model)
logistic_data$leverage<-hatvalues(model)

#logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
#write.csv(logistic_data, file = "C:\\Users\\Subhojit\\Desktop\\Logistic Regression\\Prepared by me\\pred.csv")


###########################################   Model has been build  ##############################################
###########################################   Testing on the test dataset  #######################################




# Logistic Regression on full data


modelt <- glm(Churn~ 	SeniorCitizen  
              + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
              + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
              + I(PaymentMethod=="Electronic check")+	TotalCharges , data=data.test, family=binomial())
summary(modelt)

modelt <- glm(Churn~ I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
              + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
              + I(PaymentMethod=="Electronic check")+	TotalCharges , data=data.test, family=binomial())
summary(modelt)



vif(modelt)

# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)



library(car)
library(mlogit)

# Difference between -2LL of Null model and model with variables
modelChi <- modelt$null.deviance - modelt$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modelt$df.null - modelt$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


# Hosmer and Lemeshow R square
R2.hl<-modelChi/modelt$null.deviance
R2.hl


# Cox and Snell R Square (the last number; here is 2000 should be total no. of ovservation)

R.cs <- 1 - exp ((modelt$deviance - modelt$null.deviance) /2000)
R.cs

# Max rescaled R square (Nagelkarke) (the last number; here is 2000 should be total no. of ovservation)

R.n <- R.cs /(1-(exp(-(modelt$null.deviance/2000))))
R.n



######### Lackfit Deviance ######################################################
residuals(modelt) # deviance residuals
residuals(modelt, "pearson") # pearson residuals

sum(residuals(modelt, type = "pearson")^2)
deviance(modelt)

#########Large p value indicate good model fit
1-pchisq(deviance(modelt), df.residual(modelt))

#######################################################################################
#Function - HS Test

hosmerlem <- function (y, yhat, g = 10) {
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)),
                 include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## High p value incidates the model fits well

hosmerlem(y = data.test$Churn, yhat = fitted(modelt))
################################################################################################################
# Hosmer and Lemeshow test in a different way
## High p value incidates the model fits well

library(ResourceSelection)
hl <- hoslem.test(data.test$Churn, fitted(modelt), g=10)
hl
#####################################################################################################################
# Coefficients (Odds)
modelt$coefficients
# Coefficients (Odds Ratio)
exp(modelt$coefficients)

# Predicted Probabilities
prediction <- predict(modelt,newdata = data.test,type="response")
prediction

#write.csv(prediction, file = "C:\\Users\\Subhojit\\Desktop\\Logistic Regression\\Prepared by me\\pred.csv")

library(pROC)
rocCurve   <- roc(response = data.test$Churn, predictor = prediction, 
                  levels = rev(levels(data.test$Churn)))
data.test$Churn <- as.factor(data.test$Churn)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.test$Churn)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)


#########################################################################################################################
### KS statistics calculation
data.test$m1.yhat <- predict(modelt, data.test, type = "response")

library(ROCR)
m1.scores <- prediction(data.test$m1.yhat, data.test$Churn)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

############################################################################################################

#########################################################################################################################
###################### Residual Analysis ################################################################################


logistic_data <- data.test

logistic_data$predicted.probabilities<-fitted(modelt)
logistic_data$standardized.residuals<-rstandard(modelt)
logistic_data$studentized.residuals<-rstudent(modelt)
logistic_data$dfbeta<-dfbeta(modelt)
logistic_data$dffit<-dffits(modelt)
logistic_data$leverage<-hatvalues(modelt)

#logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
#write.csv(logistic_data, file = "C:\\Users\\Subhojit\\Desktop\\Logistic Regression\\Prepared by me\\pred.csv")




#######################################################################################################

##########################################

