#1. Load the dataset into R and remove the street information in order to 
#anonymise the data. Explore the data with str and summary.

setwd("/Users/Sabrina/Downloads/")
data = read.csv("practical6.csv")

practical6$street <- NULL #remove street column, anonymize

str(practical6)

summary(practical6)

#2. Print a scatter plot of price versus each other variable so that you can 
#see which variables are likely to be most important. 

#Do you see any interesting relationship between the price and the other 
#variables?

#ANSWER : 
#Seems to be correlation between area (SqFt) and price. 

library(lattice)

attach(practical6)

data <- practical6

head(data)

forplot<-make.groups(bath=data.frame(value=bath,zipcode,city,state,price),
                     year=data.frame(value=year,zipcode,city,state,price),
                     bed=data.frame(value=bed,zipcode,city,state,price),
                     rooms=data.frame(value=rooms,zipcode,city,state,price),
                     SqFt=data.frame(value=SqFt,zipcode,city,state,price))

detach(practical6)
xyplot(price~value|which, data=forplot,scales=list(relation="free"))

#3. Randomly split the dataset into 75% train and 25% test.

num_train <- floor(0.75 * nrow(data))
#101, 120, creates values in test that do not exist in train
#149 creates warning
set.seed(149)
train_idx <- sample(seq_len(nrow(data)), size = num_train,replace = FALSE)

train <- data.frame(data[train_idx, ])
test <- data.frame(data[-train_idx, ])

#4. Fit several regression models to the training data but only using the 
#numerical attributes: a linear model (using the lm function, which fits a 
#linear model using ordinary least squares), a regression tree (CART) from 
#the rpart package (set the parameter method to anova in order to produce a 
#CART tree), and a neural network from the nnet package (set the parameters 
#skip and linout-numerical output- to TRUE and size-hidden units- to 12).

#numerical_cols <- sapply(data, is.numeric)
#lm(price ~ ., data = numerical_cols)

if (!require("rpart")) {install.packages("rpart",dependencies=TRUE);
  library("rpart")}

regression_model <- lm(price ~ zpid +  zipcode + year + bath + bed + rooms + SqFt, data = data)


set.seed(111)

cart_tree<-rpart(price~.,data=data,method="anova")

library(nnet)

neural_net <- nnet(price ~ ., data=data, skip = TRUE, linout = TRUE, size=12)

#5. View the models using the summary method and, additionally, 
#the plot method for the CART tree. Which one is the less informative?

summary(regression_model)
#here we can see that we could improve our model by e.g. removing zpid
#or rooms 
#the Adjusted R-squared is 0.36 which although a "good" R-square
#value depends on the situation, this seems very low

summary(cart_tree)
#A lot of information about the different nodes and the statistics
#on the subsamples that they represent. Mean and mean squared error for each
#node. Also information on splits and how much this improved the tree.

summary(neural_net)
#the weights and biases of the network - not very intuitive

plot(cart_tree)
#we just see the splits, no labels or anything that might help
#us know how the variables were split

#6. Prune the regression tree using the prune function and setting 
#the cp parameter to the value you consider is a good balance between 
#complexity and performance (the plotcp function plots tree sizes and 
#relative errors for different values of the complexity parameter). 
# Visualise the new tree.

bestcp <- cart_tree$cptable[which.min(cart_tree$cptable[,"xerror"]),"CP"]

# Step3: Prune the tree using the best cp.
cart_tree.pruned <- prune(cart_tree, cp = 0.01)

plotcp(cart_tree.pruned)

#7. Compare how each method works using the Root Mean Square Error (RMSE) 
#and the Mean Absolute Error (MAE) for the training and test data sets. 
#Which model performs better for the training data? And for the test data?

lm = lm(price ~., data = train)
RMSE <- sqrt(mean(lm$residuals^2))
norm_RMSE_lm <- RMSE/(max(train$price)-min(train$price))

predict_test = predict(lm, newdata = test)
RMSE_test <- sqrt(mean((test$price - predict_test)^2))
norm_RMSE_test_lm <- RMSE_test/(max(train$price)-min(train$price))

cart<-rpart(price~.,data=train,method="anova")
RMSE <- sqrt(mean(residuals(cart)^2))
norm_RMSE_cart <- RMSE/(max(train$price)-min(train$price))

predict_test = predict(cart, newdata = test)
RMSE_test <- sqrt(mean((test$price - predict_test)^2))
norm_RMSE_test_cart <- RMSE_test/(max(train$price)-min(train$price))

nn <- nnet(price ~ ., data=train, skip = TRUE, linout = TRUE, size=12)
RMSE <- sqrt(mean(residuals(nn)^2))
norm_RMSE_nn <- RMSE/(max(train$price)-min(train$price))

predict_test = predict(nn, newdata = test)
RMSE_test <- sqrt(mean((test$price - predict_test)^2))
print(RMSE_test)
norm_RMSE_test_nn <- RMSE_test/(max(train$price)-min(train$price))

#there is no "good" RMSE, it depends on the dependent variable
#which in our case is price. It is useful when comparing models
#We want the RMSE to be low, and it is a sign of overfitting
#when our test RMSE is a lot higher than our train RMSE 

#seems like the RMSE for both train and test is better for the tree

#we can also normalize the RMSE : RMSE/(max(DV)-min(DV))

#8. Can you improve the results by changing the parameters or 
#trying other methods?

#trying to change the params of the nnet

nn <- nnet(price ~ ., data=train, skip = TRUE, linout = TRUE, size=12, decay = 0.1)
RMSE <- sqrt(mean(residuals(nn)^2))
norm_RMSE_nn_20 <- RMSE/(max(train$price)-min(train$price))

predict_test = predict(nn, newdata = test)
RMSE_test <- sqrt(mean((test$price - predict_test)^2))
print(RMSE_test)
norm_RMSE_test_nn_20 <- RMSE_test/(max(train$price)-min(train$price))

#test RMSE got a little bit lower with weight decay

nn <- nnet(price ~ ., data=train, skip = TRUE, linout = TRUE, size=12, decay = 0.5)
RMSE <- sqrt(mean(residuals(nn)^2))
norm_RMSE_nn_20 <- RMSE/(max(train$price)-min(train$price))

predict_test = predict(nn, newdata = test)
RMSE_test <- sqrt(mean((test$price - predict_test)^2))
print(RMSE_test)
norm_RMSE_test_nn_20 <- RMSE_test/(max(train$price)-min(train$price))

#increasing weight decay to 0.5 made it a lot higher so a good weight decay
#is between 0.1 and 0.5

#rep is the nr of epochs
#hidden = c(5,3) means we have two hidden layers with 5 and 3 nodes
nn <- neuralnet(price ~ zpid + zipcode + year + bath + bed + rooms + SqFt,data=train,hidden=2, rep = 2)
predict_test = predict(nn, newdata = test)
RMSE_test <- sqrt(mean((test$price - predict_test)^2))
print(RMSE_test)

#doesn't get better RMSE_test = 90610.66

nn <- neuralnet(price ~ zpid + zipcode + year + bath + bed + rooms + SqFt,data=train,hidden=2, rep = 2, learningrate = 0.1)
predict_test = predict(nn, newdata = test)
RMSE_test <- sqrt(mean((test$price - predict_test)^2))
print(RMSE_test)

#not changing : RMSE_test = 90610.66

nn <- neuralnet(price ~ zpid + zipcode + year + bath + bed + rooms + SqFt,data=train,hidden=2, rep = 2, learningrate = 0.01)
predict_test = predict(nn, newdata = test)
RMSE_test <- sqrt(mean((test$price - predict_test)^2))
print(RMSE_test)

#not changing 

nn <- neuralnet(price ~ zpid + zipcode + year + bath + bed + rooms + SqFt,data=train,hidden=4, rep = 5, learningrate = 0.01)
predict_test = predict(nn, newdata = test)
RMSE_test <- sqrt(mean((test$price - predict_test)^2))
print(RMSE_test)


