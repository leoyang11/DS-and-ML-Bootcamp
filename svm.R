library(ISLR)
iris %>% head()

install.packages('e1071')
library(e1071)

model <- svm(Species ~ ., data = iris)
model
summary(model)

pred.values <- predict(model, iris[1:4])
table(pred.values, iris[,5])


tune.results <- tune(svm, train.x = iris[1:4], train.y = iris[,5], kernel = 'radial', ranges = list(cost = c(0.1, 1, 10), gamma = c(0.5,1,2)))
summary(tune.results)

#cost = 1.0, gamma = 0.5

tuned.svm <- svm(Species ~ ., data = iris, kernel = 'radial', cost = 1, gamma = 0.5)
summary(tuned.svm)


######################################
### SVM project
######################################

## load dataset
library(dplyr)
library(ggplot2)
loans <- read.csv("d:/Documents/leo/Rstudy/DS and ML Bootcamp/R-Course-HTML-Notes/loan_data.csv")
loans %>% head()

str(loans)
summary(loans)

## change variable type
loans$inq.last.6mths <- as.factor(loans$inq.last.6mths)
loans$delinq.2yrs <- as.factor(loans$delinq.2yrs)
loans$pub.rec <- as.factor(loans$pub.rec)
loans$not.fully.paid <- as.factor(loans$not.fully.paid)
loans$credit.policy <- as.factor(loans$credit.policy)

## EDA
loans %>% ggplot(aes(fico)) + 
  geom_histogram(aes(fill = not.fully.paid), color='black',binwidth = 5) + 
  theme_bw()

loans %>% ggplot(aes(purpose)) + 
  geom_bar(aes(fill = not.fully.paid), position = 'dodge') + 
  theme_bw()

loans %>% ggplot(aes(int.rate, fico)) +
  geom_point(aes(color = not.fully.paid), size = 2, alpha=0.5) + 
  theme_bw()

## train-test split
library(caTools)
split <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)

train <- subset(loans, split == T)
test <- subset(loans, split == F)

## Building the model
library(e1071)
model <- svm(not.fully.paid ~ ., data = train)
summary(model)

## predict
pred.values <- predict(model, newdata = test)
table(pred.values, test$not.fully.paid)

## tune
tune.results <- tune(svm, train.x = not.fully.paid ~ ., data = train, kernel = 'radial',
                     ranges = list(cost = c(100, 200), gamma = c(0.1, 1)))
summary(tune.results)

## tuned model
tuned.model <- svm(not.fully.paid ~ ., data = train, cost = 100, gamma = 1)
summary(tuned.model)

tuned.predictions <- predict(tuned.model, test)
table(tuned.predictions, test$not.fully.paid)









