#===============================================================================================================
#| Purpose		: Deep Learning on Classical Digit Recognition Package
#| Created by 	: Shashank Sharma
#| Subject		: Deep Learning
#===============================================================================================================

##| Setting up the working directory

setwd("C:\\DATA\\Hackathons\\08 - digit\\")
getwd()

##| importing packages

library("h2o")
library("caTools")

##| Reading datasets

train = read.csv("train.csv")
test = read.csv("test.csv")
train$label = as.factor(train$label)


##| Splitting the dataset (stratified sampling on "labels")

spl = sample.split(train$label, SplitRatio = 0.75)

Training = train[spl,]
Validation = train[!spl,]

##| Initializing and starting H2O deep learning package
##| Requires JDK
h2o.init()

##| Converting dataset into H2O environment
train1 = as.h2o(Training)

##| Using Rectifier function as activation function [F(x) = ln(1 + exp(x))]
##| F'(x) = 1 / (1 + exp(-x)) which is a logistic function


##| Preparing Labels
X = colnames(Training)
Y = X[1]
X = X[2:785]

##| Training a model
model1 <- h2o.deeplearning(X,	Y, 	train1,							# Labels and Dataset
						   activation = "RectifierWithDropout", 	# Activation Function
						   hidden = c(1024,1024, 2048), 			# Layers and Number of Neurons
						   epochs = 8000, l1 = 1e-5, 				# Maximim number of epochs and regularization
						   input_dropout_ratio = 0.2, 				# Dropout rate for input data nodes
						   train_samples_per_iteration = -1, 
						   classification_stop = -1)



						   
##| Validating the model						   
validation1 <- as.h2o(Validation)
pred1 <- h2o.predict(model1, validation1)

pred1 <- as.data.frame(pred1)
colnames(pred1) <- "label"

##| Confusion Matrix

Con_Matrix = table(Validation$label, pred1$label)

##| Accuracy Calculations
 Tsum = 0
 for (i in 1:10){Tsum = Tsum + Con_Matrix[i,i]}
 Tsum/sum(Con_Matrix)
 
##| Accuracy: 98.32%

##| Testing the model

test1 <- as.h2o(test)
pred <- h2o.predict(model1, test1)

pred1 <- as.data.frame(pred)
colnames(pred1) <- "label"

pred2 <- as.data.frame(pred1$label)
colnames(pred2) <- "label"

##| Exporting predictions in csv format

ImageID = as.data.frame(1:nrow(pred2))
OPut <- cbind(ImageID, pred2)
colnames(OPut) <- c("ImageID", "Label")
write.csv(OPut, "output1.csv",row.names = FALSE)

