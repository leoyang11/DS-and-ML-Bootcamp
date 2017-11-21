
iris %>% head()

str(iris)
colSums(is.na(iris))

var(iris[,2])
var(iris[,3])

## scale을 활용하여 표준화
iris_std <- scale(iris[,-5])
iris_std %>% head()

## 분산이 1인지 확인
var(iris_std[,2])

## iris_std와 response merge
newiris <- cbind(iris_std, iris[5])
newiris


## train-test split
library(caTools)
set.seed(101)
split <- sample.split(newiris$Species, SplitRatio = 0.7)
split %>% head(15)

train <- subset(newiris, split == T)
test <- subset(newiris, split == F)
test.y <- test[,5]


## build a KNN model
library(class)
predicted.y <- knn(train[,1:4], test[,1:4], train[,5], k=1)
predicted.y

## misclass error 확인
misclass.error <- mean(test.y != predicted.y)
misclass.error

## choosing a K value
predicted.y <- NULL
error.rate <- NULL

for (i in 1:10){
  set.seed(1710)
  predicted.y <- knn(train[,1:4], test[,1:4], train[,5], k = i)
  error.rate[i] <- mean(test.y != predicted.y)
}

error.rate

## visualization
library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate, k.values)
error.df

error.df %>% ggplot(aes(k.values, error.rate)) + 
  geom_point() + 
  geom_line(lty='dotted', color='red')



