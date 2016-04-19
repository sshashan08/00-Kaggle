##|=========================================================================================================================================
## | Created by			: Team A04 (Dawn Schnettler, Shashank Sharma, T Sai Giridhar, Yi Qin)
## | Date Created		: 2016 03 20
## | Purpose			: Collating all models for the interim presentation
## | Name				: 00 R Implementation
## | Date Updated		: _
## | Version			: v1.0
## | Previous Version	: _
##|=========================================================================================================================================

######| About:
##| 	This is a classification problem, we have to classify the income customers into 8 different classes
##|		We will primarily focus on classification algorithms, dimentionality reduction and feature engineering



## | Setting up directory

getwd()
setwd("C:/DATA/Spring 16/Applied Project/02 - Data/Prudential Life Insurance/")
getwd()


## | Importing Packages

library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
library(e1071)
library(DAAG)
library(tm)
library(xgboost)
library(dummies)
library(neuralnet)


##|===========================================================================================================================
## | Importing, exploring and cleaning datasets
##|===========================================================================================================================

##| Importing raw dataset

Inp <- read.csv("train.csv")

str(Inp)
summary(Inp)

##############| Most of the columns are numeric (numbers and binary fields)
##############| Height and Weight are being used to derive BMI
##############| "Many Dummy Variables!!"
##############| "Missing Values!!"



##| Imputing missing values using median - Function for Imputing a Data Frame's NA values with Median values

ImputeNAs <- function(DFrame)
{
  for(i in 1:ncol(DFrame))
  {
    if(is.numeric(DFrame[,i]))
    {
      DFrame[is.na(DFrame[,i]),i] <- median(DFrame[!is.na(DFrame[,i]),i])
    }
  }
 DFrame
}


##| Creating dummy variables from "Product_Info_2" column

temp = dummy(Inp$Product_Info_2, data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE)
Inp1 = data.frame(Inp[,c(1,2,4:128)],temp)

Inp1 = ImputeNAs(Inp1)

##| Partitioning Dataset for Training and Validation

spl <- sample.split(Inp1$Response , SplitRatio = 0.7)

Train <- Inp1[spl,]
Validate <- Inp1[!spl,]


##| Matrix Subsets for computations

XTRAIN = subset(Train, select = -c(Response, Id))
YTRAIN = as.factor(Train$Response)

XVALIDATE = subset(Validate, select = -c(Response, Id))
YVALIDATE = as.factor(Validate$Response)



##|===========================================================================================================================
## | Creating models
##|===========================================================================================================================


##| Using CART

model1 = rpart(Response~Product_Info_1+Product_Info_2+Product_Info_3+Product_Info_4+Product_Info_5+Product_Info_6+Product_Info_7+Ins_Age+Ht+Wt+BMI+Employment_Info_1+Employment_Info_2+Employment_Info_3+Employment_Info_4+Employment_Info_5+Employment_Info_6+InsuredInfo_1+InsuredInfo_2+InsuredInfo_3+InsuredInfo_4+InsuredInfo_5+InsuredInfo_6+InsuredInfo_7+Insurance_History_1+Insurance_History_2+Insurance_History_3+Insurance_History_4+Insurance_History_5+Insurance_History_7+Insurance_History_8+Insurance_History_9+Family_Hist_1+Family_Hist_2+Family_Hist_3+Family_Hist_4+Family_Hist_5+Medical_History_1+Medical_History_2+Medical_History_3+Medical_History_4+Medical_History_5+Medical_History_6+Medical_History_7+Medical_History_8+Medical_History_9+Medical_History_10+Medical_History_11+Medical_History_12+Medical_History_13+Medical_History_14+Medical_History_15+Medical_History_16+Medical_History_17+Medical_History_18+Medical_History_19+Medical_History_20+Medical_History_21+Medical_History_22+Medical_History_23+Medical_History_24+Medical_History_25+Medical_History_26+Medical_History_27+Medical_History_28+Medical_History_29+Medical_History_30+Medical_History_31+Medical_History_32+Medical_History_33+Medical_History_34+Medical_History_35+Medical_History_36+Medical_History_37+Medical_History_38+Medical_History_39+Medical_History_40+Medical_History_41+Medical_Keyword_1+Medical_Keyword_2+Medical_Keyword_3+Medical_Keyword_4+Medical_Keyword_5+Medical_Keyword_6+Medical_Keyword_7+Medical_Keyword_8+Medical_Keyword_9+Medical_Keyword_10+Medical_Keyword_11+Medical_Keyword_12+Medical_Keyword_13+Medical_Keyword_14+Medical_Keyword_15+Medical_Keyword_16+Medical_Keyword_17+Medical_Keyword_18+Medical_Keyword_19+Medical_Keyword_20+Medical_Keyword_21+Medical_Keyword_22+Medical_Keyword_23+Medical_Keyword_24+Medical_Keyword_25+Medical_Keyword_26+Medical_Keyword_27+Medical_Keyword_28+Medical_Keyword_29+Medical_Keyword_30+Medical_Keyword_31+Medical_Keyword_32+Medical_Keyword_33+Medical_Keyword_34+Medical_Keyword_35+Medical_Keyword_36+Medical_Keyword_37+Medical_Keyword_38+Medical_Keyword_39+Medical_Keyword_40+Medical_Keyword_41+Medical_Keyword_42+Medical_Keyword_43+Medical_Keyword_44+Medical_Keyword_45+Medical_Keyword_46+Medical_Keyword_47+Medical_Keyword_48, data = Train1, method = "class")

summary(model1)

## Variable importance
##                BMI                 Wt Medical_History_23  Medical_History_4 Medical_Keyword_15 	 Medical_History_15     Product_Info_4     Product_Info_2 Medical_Keyword_48  Medical_History_6  Medical_History_33 Medical_Keyword_23 Medical_Keyword_25 
##                 30                 15                 12                 10                 10 					 8                  4                  2                  2                  2 

pred1 <- predict(model1, newdata = XVALIDATE, type = "class")

##| Confusion Matrix
table(YVALIDATE, pred1)

##| Using SVM

model2 <- svm(as.matrix(XTRAIN) ,y = as.matrix(YTRAIN), scale = TRUE, type = "C-classification", kernel = "radial", degree = 3, cross= 5, gamma = 1/ncol(XTRAIN), cost = 3.2, cachesize = 200)

pred2 <- predict(model2, newdata = as.matrix(XVALIDATE))

table(YVALIDATE, pred2)


XF = rbind(XTRAIN,XVALIDATE)
YF = c(YTRAIN,YVALIDATE)



model2F <- svm(as.matrix(XF) ,y = as.matrix(YF), scale = TRUE, type = "C-classification", kernel = "radial", degree = 3, gamma = 1/ncol(XTRAIN), cross= 5,cost = 3.2, cachesize = 200)

##| Using on test dataset

InpT = read.csv("test.csv")

temp = dummy(InpT$Product_Info_2, data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE)
InpT1 = data.frame(InpT[,c(1,2,4:127)],temp)

InpT1 = ImputeNAs(InpT1)

pred2F <- predict(model2F, as.matrix(InpT1)




##| Using Neural Networks

# cols <- columnnames(XTRAIN)

YTRAIN = as.numeric(YTRAIN)
Data_NN <- cbind(XTRAIN, YTRAIN)

model3 <- neuralnet(YTRAIN ~ BMI + Employment_Info_1 + Employment_Info_2 + Employment_Info_3 + Employment_Info_4 + Employment_Info_5 + Employment_Info_6 + Family_Hist_1 + Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5 + Ht + Ins_Age + Insurance_History_1 + Insurance_History_2 + Insurance_History_3 + Insurance_History_4 + Insurance_History_5 + Insurance_History_7 + Insurance_History_8 + Insurance_History_9 + InsuredInfo_1 + InsuredInfo_2 + InsuredInfo_3 + InsuredInfo_4 + InsuredInfo_5 + InsuredInfo_6 + InsuredInfo_7 + Product_Info_1 + Product_Info_2A1 + Product_Info_2A2 + Product_Info_2A3 + Product_Info_2A4 + Product_Info_2A5 + Product_Info_2A6 + Product_Info_2A7 + Product_Info_2A8 + Product_Info_2B1 + Product_Info_2B2 + Product_Info_2C1 + Product_Info_2C2 + Product_Info_2C3 + Product_Info_2C4 + Product_Info_2D1 + Product_Info_2D2 + Product_Info_2D3 + Product_Info_2D4 + Product_Info_2E1 + Product_Info_3 + Product_Info_4 + Product_Info_5 + Product_Info_6 + Product_Info_7 + Wt, data = as.matrix(Data_NN), hidden = c(80,16), threshold = 0.1, linear.output = FALSE, stepmax = 1e+05, learningrate.factor = list(minus = 0.5, plus = 1.2),learningrate= 0.01, lifesign = "none", algorithm = "backprop")

plot(model3)

pred3 <- compute(model3, XVALIDATE)

YVALIDATE = as.numeric(YVALIDATE)
VData_NN = cbind(XVALIDATE,YVALIDATE)

pred3 <- compute(model3, XVALIDATE)


table(YVALIDATE, pred3)




##| Using XGBoost


params <- list(booster = "gbtree", objective = "multi:softmax", num_class=8, eta = 0.01, max_depth = 10, subsample = 0.75, num_parallel_tree= 10)

tmp <- xgb.cv(params = list(), as.matrix(XTRAIN), nrounds = 1000, nfold = 10, label = YTRAIN, missing = NULL, prediction = FALSE, showsd = TRUE, obj = NULL, feval = NULL, stratified = TRUE, verbose = TRUE, print.every.n = 1L, early.stop.round = 50,  maximize = TRUE)

model10 <- xgboost(params =  params, data = as.matrix(XTRAIN), label = as.matrix(YTRAIN), nrounds = 35, nfold = 10, maximize = TRUE)

pred10 <- predict(model10, as.matrix(XVALIDATE))

pred10 = as.factor(as.numeric(pred10)+1)

#|Confusion Matrix
table(YVALIDATE, pred10)

##| Using on test data

InpT = read.csv("test.csv")

temp = dummy(InpT$Product_Info_2, data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE)
InpT1 = data.frame(InpT[,c(1,2,4:127)],temp)

InpT1 = ImputeNAs(InpT1)

predF <- predict(model10, as.matrix(InpT1))

predF = as.factor(as.numeric(predF)+1)
out = data.frame(InpT[,1],predF)
write.csv(out, "svm.csv", row.names = FALSE)

