install.packages("ISLR")
library(ISLR)
library(dplyr)

Caravan %>% head()
str(Caravan)
summary(Caravan)

## 결측치 체크
colSums(is.na(Caravan))

## var 확인 : 분산이 제각각임. 이런 경우 표준화가 꼭 필요
var(Caravan[,1])
var(Caravan[,2])

## purchase는 예측대상이 되는 종속변수이므로 표준화에서 제외
purchase <- Caravan$Purchase

## 표준화
standardized.Caravan <- scale(Caravan[, -86])
standardized.Caravan %>% head()

var(standardized.Caravan[,1])  # 표준화하면 분산이 1로 고정
var(standardized.Caravan[,2])

## Train Test Split
idx <- 1:1000
test.data <- standardized.Caravan[idx,]
test.purchase <- purchase[idx]

train.data <- standardized.Caravan[-idx,]
train.purchase <- purchase[-idx]

## KNN model
library(class)
set.seed(101)

predicted.purchase <- knn(train.data, test.data, train.purchase, k=5)
predicted.purchase %>% head()

## misclass error 확인
misclass.error <- mean(test.purchase != predicted.purchase)
misclass.error

## choosing a K value
predicted.purchase <- NULL
error.rate <- NULL

for (i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data, test.data, train.purchase, k = i)
  error.rate[i] <- mean(test.purchase != predicted.purchase)
}

error.rate

## visualize K elbow method
library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)
error.df

error.df %>% ggplot(aes(k.values, error.rate)) + 
  geom_point() + 
  geom_line(lty='dotted', color='red')






