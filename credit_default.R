# AMBER GARNER
# data from: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
# calculate probability of default (PD) for credit card customers
# 
# variables include:
#         LIMIT_BAL: Amount of the given credit (NT dollar)
#         SEX: (1 = male; 2 = female).
#         Education (1 = graduate school; 2 = university; 3 = high school; 4 = others).
#         Marital status (1 = married; 2 = single; 3 = others).
#         Age (year)
#         PAY_0-6: History of past payment.-2 = pay duly 2 months; -1 = pay duly 1 month; 0 = on time; 1 = payment delay for one month; 2 = payment delay for two months; . .; 8 = payment delay for eight months; 9 = payment delay for nine months and above. 
#         PAY_AMT1-6: Amount of previous payment (NT dollar). X18 = amount paid in September, 2005; X19 = amount paid in August, 2005; . . .;X23 = amount paid in April, 2005. 
#         BILL_AMT1-6: Amount of bill statement (NT dollar). X12 = amount of bill statement in September, 2005; X13 = amount of bill statement in August, 2005; . . .; X17 = amount of bill statement in April, 2005.
#         default.payment.next.month: 0= Did not default; 1= default       


# load necessary packages
options(scipen=999)
library(gmodels)
library(plyr)
library(corrplot)
library(DMwR)
library(caret)
library(ROCR)
library(ggplot2)
library(rpart)
library(ipred)
library(randomForest)

setwd("C:/Users/amber/Desktop/Credit Risk Model")
dir()

#***********DATA PREP******************************

CR<-read.csv("CRedit_default.csv", header = TRUE)
head(CR)
str(CR)
# check correlations
M <- cor(CR)
corrplot(M, method = "circle")
# multicollinearity is low 
#Pay status is the most correlated with default 


# change target variable name
CR$default<-CR$default.payment.next.month
CR$default.payment.next.month<-NULL

# convert categorical variables to factors
CR$SEX<-as.factor(CR$SEX)
CR$EDUCATION<-as.factor(CR$EDUCATION)
CR$MARRIAGE<-as.factor(CR$MARRIAGE)
CR$PAY_0<-as.factor(CR$PAY_0)
CR$PAY_2<-as.factor(CR$PAY_2)
CR$PAY_3<-as.factor(CR$PAY_3)
CR$PAY_4<-as.factor(CR$PAY_4)
CR$PAY_5<-as.factor(CR$PAY_5)
CR$PAY_6<-as.factor(CR$PAY_6)
CR$default<-as.factor(CR$default)
# look for errors in categorical variables 
CrossTable(CR$MARRIAGE)# 1=MARRIED; 2=SINGLE; 3=OTHER
# marriage contains a level 0 with 0.002% of values
# remomve rows where the marriage is unknown
CR<-CR[!CR$MARRIAGE==0,]
# look for errors in sex
CrossTable(CR$SEX)# 1=MALE; 2=FEMALE.....74% are female
# look for errors in education
CrossTable(CR$EDUCATION)# 1 = graduate school; 2 = university; 3 = high school; 4 = others...82% high school
CrossTable(CR$default)# 0= not default; 1=default....91% do not default...unbalanced target

# check for missing values and look at distributions
summary(CR) # no missing values


# change category names for easier anlaysis
CR$SEX<-revalue(CR$SEX, c("1"="MALE", "2"="FEMALE"))
CR$MARRIAGE<-revalue(CR$MARRIAGE, c("1"="MARRIED", "2"="SINGLE", "3"="OTHER"))
CR$EDUCATION<-revalue(CR$EDUCATION, c("1"="GRAD SCHOOL", "2"="UNDERGRAD", "3"="HIGH SCHOOL", "4"="OTHER"))
CR$PAY_0<-revalue(CR$PAY_0, c("-2"="PAY DULY 2 MONTHS", "-1"="PAY DULY 1 MONTH","0"="ON TIME","1"="1 MONTH", "2"="2 MONTHS", "3"="3 MONTHS", 
                              "4"="4 MONTHS", "5"="5 MONTHS", "6"="6 MONTHS", "7"="7 MONTHS", "8"="8 MONTHS"))
CR$PAY_2<-revalue(CR$PAY_2, c("-2"="PAY DULY 2 MONTHS", "-1"="PAY DULY 1 MONTH","0"="ON TIME", "1"="1 MONTH", "2"="2 MONTHS", "3"="3 MONTHS", 
                              "4"="4 MONTHS", "5"="5 MONTHS", "6"="6 MONTHS", "7"="7 MONTHS", "8"="8 MONTHS"))
CR$PAY_3<-revalue(CR$PAY_3, c("-2"="PAY DULY 2 MONTHS", "-1"="PAY DULY 1 MONTH","0"="ON TIME", "1"="1 MONTH", "2"="2 MONTHS", "3"="3 MONTHS", 
                              "4"="4 MONTHS", "5"="5 MONTHS", "6"="6 MONTHS", "7"="7 MONTHS", "8"="8 MONTHS"))                 
CR$PAY_4<-revalue(CR$PAY_4, c("-2"="PAY DULY 2 MONTHS", "-1"="PAY DULY 1 MONTH","0"="ON TIME", "1"="1 MONTH", "2"="2 MONTHS", "3"="3 MONTHS", 
                              "4"="4 MONTHS", "5"="5 MONTHS", "6"="6 MONTHS", "7"="7 MONTHS", "8"="8 MONTHS"))
CR$PAY_5<-revalue(CR$PAY_5, c("-2"="PAY DULY 2 MONTHS", "-1"="PAY DULY 1 MONTH","0"="ON TIME", "1"="1 MONTH", "2"="2 MONTHS", "3"="3 MONTHS", 
                              "4"="4 MONTHS", "5"="5 MONTHS", "6"="6 MONTHS", "7"="7 MONTHS", "8"="8 MONTHS"))
CR$PAY_6<-revalue(CR$PAY_6, c("-2"="PAY DULY 2 MONTHS", "-1"="PAY DULY 1 MONTH","0"="ON TIME", "1"="1 MONTH", "2"="2 MONTHS", "3"="3 MONTHS", 
                              "4"="4 MONTHS", "5"="5 MONTHS", "6"="6 MONTHS", "7"="7 MONTHS", "8"="8 MONTHS"))

# look at the distribution of default based on education
CrossTable(CR$EDUCATION, CR$default, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) 
# grad school education has a 0% default 
# highest default rate amoung undergrad graduates at 17%


# look at the distribution of default based on gender
CrossTable(CR$SEX, CR$default, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) 
# males are twice as likely to default. female default: 7.5%; male default 14.3%


# look at the distribution of default based on marital status
CrossTable(CR$MARRIAGE, CR$default, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# marital status doesn't have much effect on default

# look at pay status variables
CrossTable(CR$PAY_0, CR$default, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# when pay_0 is 3 months past due, the probability of default is highest at 76%
CrossTable(CR$PAY_6, CR$default, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# when pay_6 is 8 months of more past due the probability of default is 100%


# look for outliers 
hist(CR$AGE) # 25-30 is the most common age range; very few above the age of 50
plot(CR$AGE)
# remove outlier
CR<-CR[CR$AGE<70,]

hist(CR$LIMIT_BAL) 
# change breaks to get a clearer view
n_breaks<-sqrt(nrow(CR))
hist(CR$LIMIT_BAL, breaks = n_breaks) # there are outliers
plot(CR$LIMIT_BAL)
# remove outliers
CR<-CR[CR$LIMIT_BAL<800000,]

# check limit balance for outliers
plot(CR$LIMIT_BAL)

#create a data partition of 70% and 30% of data
set.seed(1234)
splitIndex <- createDataPartition(CR$default, p = .70,
                                  list = FALSE,
                                  times = 1)

# create training data set (70%) and test data set (30%)
training <- CR[ splitIndex,]
test <- CR[-splitIndex,]

# check that probabilities are the same in the new data sets as original
prop.table(table(training$default)) # 78% not default
table(training$default)
prop.table(table(test$default)) # 78% not default

#*************LOGISTIC REGRESSION**************************************
# remove id variable
training$ID<-NULL

# train model
model<-glm(default~.-ID, data=training, family=binomial)
summary(model)
# get predictions
predict <- predict(model, type = 'response')
# create confusion matrix
table(training$default, predict > 0.5)
tp<-1671
fn<-15568
tn<-738
fp<-2966
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision

# change cutoff to 0.3
# create confusion matrix
table(training$default, predict > 0.3)
tp<-2314
tn<-1668
fp<-2323
fn<-14638

acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision

# change cutoff to 0.2
# create confusion matrix
table(training$default, predict > 0.2)
tn<-3398
tp<-2906
fn<-12908
fp<-1731
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision

predict<-predict(model, test, type="response")
table(test$default, predict > 0.2)
tn<-1487
tp<-1198
fn<-5501
fp<-789
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision


#*************BAGGING******************************
# bagging
set.seed(3334)
bag <- bagging(default ~., data=training, coob=TRUE, nbagg=100)
predictions <- predict(bag)
bag_train_conf<-table(predictions, training$default)
predictions<-predict(bag, test)
bag_test_conf<-table(predictions, test$default)
bag_train_conf
bag_test_conf
tn<-6549
tp<-754
fn<-1233
fp<-439
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision


# random forest
set.seed(113)
rf<-randomForest(default~.,data=training, ntree=200, corr.bias=TRUE)
predictions <- predict(rf)
rf_train_conf<-table(predictions, training$default)
predictions<-predict(bag, test)
rf_test_conf<-table(predictions, test$default)
rf_train_conf
rf_test_conf
tn<-6547
tp<-752
fn<-1235
fp<-441
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision