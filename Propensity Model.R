# Propensity Model 

library(dplyr)
library(tidyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sqldf)

#Load the data 
ccmodel <- read.csv("C://Users//sushil//OneDrive//MMA learning//831 Marketing Analytics//Final Project//CC GENERAL.csv")
str(ccmodel)
summary(ccmodel)

#Identified NA's CREDIT_LIMIT, MINIMUM_PAYMENTS - Imputations 

#Create dummy on Cash_Advance - Propensity to use cash advance 
library(glmnet)
ccmodel$DUMMY_CASH_ADVANCE <- ifelse(ccmodel$CASH_ADVANCE == 0, 0, 1)
View(ccmodel)
summary(ccmodel)

#Checking normality 
plot(density(ccmodel.clean$BALANCE))
library("ggpubr")
ggdensity(ccmodel.clean$BALANCE, 
          main = "Density plot of Balance",
          xlab = "Cr. Balance")
hist(ccmodel.clean$CASH_ADVANCE) 

#Omit NA 
ccmodel.clean <- na.omit(ccmodel)
ccmodel.clean$CUST_ID <- NULL
summary(ccmodel.clean)
str(ccmodel.clean)


#Data exploration 
explore <- ggplot(ccmodel.clean, aes(x = CREDIT_LIMIT, y=PURCHASES))+
  geom_point(aes(color= CREDIT_LIMIT), size = 2)
explore 

ggplot(data = ccmodel.clean, aes(x = CREDIT_LIMIT, y = BALANCE)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = FALSE)


str(ccmodel.clean)

ggplot2.scatterplot(data=ccmodel.clean, xName='BALANCE',yName='CREDIT_LIMIT',
                    groupName="Bal vs Cr_Limit")

#Checking coll for two variables individually 
cor(ccmodel.clean$BALANCE, ccmodel.clean$CREDIT_LIMIT)

str(ccmodel.clean)
summary(ccmodel.clean)

#Separate data in train and test 
library(caTools)
set.seed(123)
sample = sample.split(ccmodel.clean,SplitRatio = 0.75)
train = subset(ccmodel.clean,sample ==TRUE)
test = subset(ccmodel.clean, sample==FALSE)
str(testX)

testX = test[c(1:17)]; # predictors in the testing set
actual.test.cash_a = test[18]; # responses in the testing set

# build a logistic regression model to predict Cash Advance using the training data. 
# We can now use "." in place of enumerating all the remaining independent variables in the following way:

#All variables except cash_advance 
log_cashadvance <- glm(DUMMY_CASH_ADVANCE~BALANCE+BALANCE_FREQUENCY+PURCHASES+ONEOFF_PURCHASES
                       +INSTALLMENTS_PURCHASES+PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY
                       +PURCHASES_INSTALLMENTS_FREQUENCY+PURCHASES_TRX+CREDIT_LIMIT+PAYMENTS
                       +MINIMUM_PAYMENTS+PRC_FULL_PAYMENT+TENURE, data=train, family=binomial)
summary(log_cashadvance) 

#Only significant variables 

log_cashadvance1 <- glm(DUMMY_CASH_ADVANCE~BALANCE*BALANCE_FREQUENCY+PURCHASES+
                        +ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+CREDIT_LIMIT+PAYMENTS
                        +MINIMUM_PAYMENTS+PRC_FULL_PAYMENT+TENURE, data=train, family=binomial)

plot(log_cashadvance1) 

summary(log_cashadvance1)

#Make predictions on the test set by using the command:
testPredict = predict(log_cashadvance1, newdata=test, type="response")
testPredict

library(pROC) 
test_prob = predict(log_cashadvance1, newdata = test, type = "response")
test_roc = roc(test$DUMMY_CASH_ADVANCE ~ test_prob, plot = TRUE, print.auc = TRUE)
df_output <- data.frame(test$CUST_ID, test_prob)
write.csv(df_output, file = "cash_advance_final.csv")
plot(density(df_output$test_prob))

write.csv(test_prob, file = "cash_advance.csv") # export the predicted prices of lasso into a CSV file

#Generate Confusion matrix at 50% threshold 
# Then, we can create a confusion matrix with a threshold of 0.50 by using the table command:
confusion.matrix<-table(test$DUMMY_CASH_ADVANCE, testPredict >= 0.50)
confusion.matrix

# The accuracy of the model is? 
Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]
Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]

Accuracy.rate<-Count.correct/(Count.correct+Count.wrong)
Accuracy.rate
# What is the prediction accuracy of the model? 80% Accuracy which is not bad 

#Impact of variables on outcome variables 

1-exp(0.004616)
1-exp(2.195e+00)
1-exp(-4.832e-04)
1-exp(-1.013e+00)
1-exp(-1.715e+00)
1-exp(-1.328e-04)
1-exp(4.404e-04)
exp(-8.096e-05)
1-exp(-1.107e+00)
1-exp(-1.753e-01)
1-exp(-3.986e-03)

################################ PCA ######################################################

library(pls)
set.seed(2)

pcr.ca.fit = pcr(DUMMY_CASH_ADVANCE ~., data=train, center = TRUE, scale=TRUE, validation="CV")
summary(pcr.ca.fit)

pcr.ca.pred <- predict(pcr.ca.fit, test, ncomp = 5) 

test_roc_pca = roc(test$DUMMY_CASH_ADVANCE ~ pcr.ca.pred, plot = TRUE, print.auc = TRUE)

################################## LASSO ################################################# 

train$CUST_ID <- NULL
test$CUST_ID <- NULL
testX$CUST_ID <- NULL
View(testX)
X.Train_CA = model.matrix(DUMMY_CASH_ADVANCE ~ . ,data = train)[,-1]
summary(train)
y = train$DUMMY_CASH_ADVANCE

library(glmnet)

ca.lasso <- glmnet(X.Train_CA, y, family ='binomial', alpha = 1)
cv <- cv.glmnet(X.Train_CA, y, family ='binomial', alpha = 1)
plot(cv)

penalty.lasso <- cv$lambda.min
log(penalty.lasso)

lasso.opt.fit <-glmnet(X.Train_CA, y, alpha = 1, lambda = penalty.lasso)
coef(lasso.opt.fit)

testX <- as.matrix(testX)
str(testX)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =testX))

coef(lasso.opt.fit)

test_prob_lasso = predict(lasso.opt.fit, newx = testX, type = "response")
str(test_prob_lasso)
test_roc = roc(test$DUMMY_CASH_ADVANCE ~ test_prob_lasso, plot = TRUE, print.auc = TRUE)

write.csv(test_prob_lasso, file = "ca_lasso.csv") # export the predicted prices of lasso into a CSV file






