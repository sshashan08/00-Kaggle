#===============================================================================================================
#| Purpose		: Kaggle Titanic Problem
#| Created by 	: Shashank Sharma
#===============================================================================================================

library(randomForest)
library(caTools)
library(caret)
library(rpart)
library(rpart.plot)



getwd()
setwd("C:/DATA/Hackathons/06 - Titanic")
getwd()
titanic = read.csv("train.csv")
str(titanic)
test = read.csv("test.csv")
summary(titanic)

spl = sample.split(titanic$Survived, SplitRatio = 0.8)
train = titanic[spl,]
validate = titanic[!spl,]

train$Survived = as.factor(train$Survived)
validate$Survived = as.factor(validate$Survived)
train$Age[is.na(train$Age)] = mean(train$Age, , na.rm = TRUE)
validate$Age[is.na(validate$Age)] = mean(validate$Age, , na.rm = TRUE)
test$Age[is.na(test$Age)] = mean(test$Age, na.rm = TRUE)

model1 = rfcv(trainx = train[,c(3,5,6,7,8,10,12)], trainy = train[,2], cv.fold = 10, scale = "log", step = seq(0.5,1,0.1), mtry = function(p) max(1, floor(sqrt(p))), recursive = TRUE)
str(train)


model1 = rfcv(trainx = train[,c(3,5,6,7,8,10,12)], trainy = train[,2], cv.fold = 10, scale = "log", step = 0.5, mtry = function(p) max(1, floor(sqrt(p))), recursive = FALSE)
str(train)
is.na(train$Embarked)
model1 = rfcv(trainx = train[,c(3,5,6,7,8,10,12)], trainy = train[,2], cv.fold = 10, scale = "log", step = 0.5, mtry = function(p) max(1, floor(sqrt(p))), recursive = FALSE)
summary(train)
is.na(train$Age)
train$Age[is.na(train$Age)] = mean(train$Age)
is.na(train$Age)
train$Age
train$Age[is.na(train$Age)] = mean(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)] = mean(test$Age, na.rm = TRUE)
validate$Age[is.na(validate$Age)] = mean(validate$Age, na.rm = TRUE)
is.na(train$Age)
model1 = rfcv(trainx = train[,c(3,5,6,7,8,10,12)], trainy = train[,2], cv.fold = 10, scale = "log", step = 0.5, mtry = function(p) max(1, floor(sqrt(p))), recursive = FALSE)
model1

library(rpart)
library(rpart.plot)
model2 = rpart(Survived	~	Pclass+Sex+Age+SibSp+Parch+Fare+Cabin+Embarked, data = train, minbucket = 5, method = "class")
prp(model2)
fitctrl = trainControl(method = "LOOCV", number = 10)
??trainControl
library(caret)

fitctrl = trainControl(method = "LOOCV", number = 10)
fitgrid = expand.grid(.cp = seq(0.1,0.5,0.01))
train(Survived	~	Pclass+Sex+Age+SibSp+Parch+Fare+Cabin+Embarked, data = train, method = "rpart", trControl = fitctrl, tunegrid = fitgrid)


model2 = rpart(Survived	~	Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data = train, minbucket = 5, method = "class")
prp(model2)
pred = predict(model2, newdata = test, type = "class")
op = data.frame(test$PassengerId,pred, row.names = NULL)
write.csv(op, "opt.csv")