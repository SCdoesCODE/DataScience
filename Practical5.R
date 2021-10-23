#LOADING DATA
setwd("/Users/Sabrina/Downloads/")
wine<-read.table("wine.data.txt",header=F,sep=',')

#ANALYSING DATA
str(wine)

#The first column is the class which has been considered as numeric. 
#Hence, our next step is to change the names of the columns (to make 
#them more informative) and the type of the target variable.
t<-
  c("class","alco","ma","ash","alc","mg","tp","flav","noflav","proa","co
l","hue","od","prol")
names(wine)<-t
wine$class=as.factor(wine$class)
head(wine)

#We can also show some plots:
if (!require("ggplot2"))
{install.packages("ggplot2",dependencies=TRUE); library("ggplot2")}
## Loading required package: ggplot2
## Warning: package 'ggplot2' was built under R version 3.2.2
qplot(alco,mg,data=wine,col=class, xlab="Alcohol",ylab="Magnesium")

pairs(wine[,2:6],col=wine$class)

#LEARNING A MODEL
#We learn a Decision Tree using the rpart package:
if (!require("rpart")) {install.packages("rpart",dependencies=TRUE);
  library("rpart")}
## Loading required package: rpart
colnames(wine)
colnames(wine) <- make.names(colnames(wine))
model<-rpart(class~.,data=wine,method="class")

#The tree can be graphically visualised using plot and text:
plot(model, main="Classification Tree for Wine dataset")
text(model,use.n=TRUE, all=TRUE, cex=.8)

#We can also display the model
printcp(model)
#rel error is the training error
#xerror is the cross validated error
#we can prune the tree based on the info given by the printcp function

#1.- Randomly split the dataset into 75% train and 25% test. 
#Note: It can be done by using the sample function to generate 
#integers belonging to the interval [1..size(dataset)]. 
#Use these numbers to identify the instances.

n_test <- floor(0.75 * nrow(wine))

#set seed so that we can reproduce
set.seed(123)
train_idx <- sample(seq_len(nrow(wine)), size = n_test)

train <- wine[train_idx, ]
test <- wine[-train_idx, ]

#2.- Learn a decision tree using the training set.
model_train<-rpart(class~.,data=train,method="class")


#3.- Visualise the tree and display the results. 
#Is there any difference with respect to the
#model trained with the whole dataset?

plot(model, main="Classification Tree for Wine dataset")
text(model,use.n=TRUE, all=TRUE, cex=.8)
printcp(model)

plot(model_train, main="Classification Tree for Wine dataset (train data)")
text(model_train,use.n=TRUE, all=TRUE, cex=.8)
printcp(model_train)

#ANSWER : root node error for train : 82/133 = 0.61654
#root node error for whole dataset : 107/178 = 0.60112

#4.- Use the model to predict the class label for the test set by using the 
#"predict" function. Repeat the predictions but now using the parameter 
#type="class" (use a different variable to keep the new results). 
#What are the differences?

predict_test = predict(model_train,newdata = test)
predict_test
#get column names of the max values in prediction
#predict_test returns the prob of each column
predictions <- apply(X=predict_test, MARGIN=1, FUN=which.max)
#actual value
ground_truth <- wine$class[-train_idx]

#number of correct predictions
correct <- sum(predictions == ground_truth)
#number of incorrect predictions
incorrect <- sum(predictions != ground_truth)

#errar fraction
err <- incorrect/(correct + incorrect)

#WITH type = "class"
predict_test = predict(model_train,newdata = test, type ="class")
predict_test
#WE CAN skip this step of taking the max now that we use type = "class"
#predictions <- apply(X=predict_test, MARGIN=1, FUN=which.max)
ground_truth <- wine$class[-train_idx]
correct <- sum(predict_test == ground_truth)
incorrect <- sum(predict_test != ground_truth)
err <- incorrect/(correct + incorrect)

#5.- Calculate the performance of the model when it is applied to the 
#test set by displaying a table that shows the predicted classes versus 
#the real classes.

data.frame(predict_test, ground_truth)

#confusion matrix
table(predict_test, ground_truth)

#pred_table <- data.frame(predict_test, ground_truth)
#library(gridExtra)
#library(grid)
#grid.table(pred_table)

#6.- Try some other methods or parameters of the rpart package to see 
#whether you can still improve the results further. You can also compare 
#with other packages.

#I'm choosing to look at the last (iteration) xerror (cross validation error)

#https://stats.stackexchange.com/questions/215290/performance-of-regression-tree-rpart

set.seed(10)

model<-rpart(class~.,data=train,method="class")
printcp(model) #last xerror is 0.21

#in this case we are only using one attribute proa
#and the tree seems to be doing a lot of different partitions
#seems like it is overfitting
model2<-rpart(class~proa,data=train[,c("class","proa")],method="class")
plot(model2, main="Classification Tree for Wine dataset")
printcp(model2) #last xerror is 0.76

model3<-rpart(class~flav+ hue + od  + prol,data=train,method="class")
plot(model3, main="Classification Tree for Wine dataset")
printcp(model3) #last xerror seems to be lower than when we choose all classes

model4<-rpart(class~flav+ hue + od  + prol,data=train,method="poisson")
plot(model4, main="Classification Tree for Wine dataset")
printcp(model4) #last xerror seems to  also be low with poisson

model4<-rpart(class~flav+ hue + od  + prol,data=train,method="anova")
plot(model4, main="Classification Tree for Wine dataset")
printcp(model4) #last xerror seems to be low with anova as well

#TESTING : change the model
predict_test = predict(model3,newdata = test, type ="class")
predict_test
#WE CAN skip this step of taking the max now that we use type = "class"
#predictions <- apply(X=predict_test, MARGIN=1, FUN=which.max)
ground_truth <- wine$class[-train_idx]
correct <- sum(predict_test == ground_truth)
incorrect <- sum(predict_test != ground_truth)
err <- incorrect/(correct + incorrect)

#BAGGING to decrease variance
#generating training sets by choosing samples from dataset
#with repetition - one method is random forests, ensemble method
#combining multiple simple models
library(randomForest)
wine.rf <- randomForest(class ~ ., data=train, importance=TRUE,
                        proximity=TRUE)
wine.rf

#OOB estimate of  error rate: 2.26%

wine.rf2 <- randomForest(class~flav+ hue + od  + prol, data=train, importance=TRUE,
                        proximity=TRUE)

wine.rf2
#OOB estimate of  error rate: 5.26%

wine.rf3 <- randomForest(class~flav+ hue + od  + prol, data=train, importance=TRUE,
                         proximity=TRUE, ntree = 300)

wine.rf3

#OOB estimate of  error rate: 6.02%

wine.rf4 <- randomForest(class~flav+ hue + od  + prol, data=train, importance=TRUE,
                         proximity=TRUE, ntree = 1000)

wine.rf4

#OOB estimate of  error rate: 5.26%

wine.rf4 <- randomForest(class~., data=train, importance=TRUE,
                         proximity=TRUE, ntree = 1000)

wine.rf4

#OOB estimate of  error rate: 2.26% - more trees don't help

wine.rf5 <- randomForest(class~., data=train, importance=TRUE,
                         proximity=TRUE, ntree = 1000, maxnodes=12)

wine.rf5

#OOB estimate of  error rate: 2.26% - tried diff maxnodes from 3-12


#TESTING : change the model
predict_test = predict(wine.rf,newdata = test, type ="class")
predict_test
#WE CAN skip this step of taking the max now that we use type = "class"
#predictions <- apply(X=predict_test, MARGIN=1, FUN=which.max)
ground_truth <- wine$class[-train_idx]
correct <- sum(predict_test == ground_truth)
incorrect <- sum(predict_test != ground_truth)
err <- incorrect/(correct + incorrect)


#caret package
