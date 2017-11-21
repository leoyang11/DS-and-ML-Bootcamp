library(dplyr)
library(ggplot2)

df_train <- read.csv('d:/Documents/leo/Rstudy/DS and ML Bootcamp/R-Course-HTML-Notes/titanic_train.csv')
df_train %>% tail
str(df_train)

## EDA 및 데이터 전처리
install.packages('Amelia')
library(Amelia)

## CHK missing data
missmap(df_train, main = 'Missing Map', col = c('yellow', 'black'), legend = F)

## plotting
df_train %>% ggplot(aes(Survived)) + geom_bar()
df_train %>% ggplot(aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))
df_train %>% ggplot(aes(Sex)) + geom_bar(aes(fill=factor(Sex)))
df_train %>% ggplot(aes(Pclass)) + geom_bar(aes(fill=factor(Survived)))
df_train %>% ggplot(aes(Sex)) + geom_bar(aes(fill=factor(Survived)))

df_train %>% ggplot(aes(Age)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'navy')
df_train %>% ggplot(aes(Fare)) + geom_histogram(fill='green', color='black', alpha = 0.5)

## Age에 있는 missing value 처리 -> class별로 평균 나이 넣기.
df_train %>% ggplot(aes(Pclass, Age)) + geom_boxplot(aes(group=Pclass, fill=factor(Pclass), alpha=0.4)) + scale_y_continuous(breaks = seq(0,80,by=5))

## class 별 age
df_train %>% 
  group_by(Pclass) %>% 
  summarise(mean.Age = mean(Age, na.rm=T))

impute_age <- function(age, class){
  z <- age
  for (i in 1:length(age)){
    if (is.na(age[i]))
      if (class[i] == 1) {
        z[i] <- 38
      }else if(class[i] == 2){
        z[i] <- 29
      }else{
        z[i] <- 25
  }else{
      z[i] <- age[i]
    }
  }
  return(z)
}

## fixed ages 계산하기 (위에서 만든 impute_age 함수 이용)
fixed_ages <- impute_age(df_train$Age, df_train$Pclass)

## Age 수정하기
df_train$Age <- fixed_ages

## missmap 재확인
missmap(df_train, main = 'Missing Map', col = c('yellow', 'black'), legend = F)

## 미사용 컬럼 제거
df_train2 <- df_train %>% select(-PassengerId, -Name, -Ticket, -Cabin)
df_train2 %>% head()

## 데이터형 factor로 변환
df_train2$Survived <- factor(df_train2$Survived)
df_train2$Pclass <- factor(df_train2$Pclass)
df_train2$Parch <- factor(df_train2$Parch)
df_train2$SibSp <- factor(df_train2$SibSp)


## train the model
log.model <- glm(formula = Survived ~ ., family = binomial(link = 'logit'), data = df_train2)

summary(log.model)

## Predicting using Test Cases
library(caTools)
split <- sample.split(df_train2$Survived, SplitRatio = 0.70)
final.train <- subset(df_train2, split == T)
final.test <- subset(df_train2, split == F)

final.log.model <- glm(formula = Survived ~ ., family = binomial(link = 'logit'), data = final.train)

summary(final.log.model)

## Predict!!!
fitted.probabilities <- predict(final.log.model, newdata = final.test, type = 'response')

fitted.result <- ifelse(fitted.probabilities > 0.5, 1, 0)

misClasificError <- mean(fitted.result != final.test$Survived)
1 - misClasificError

table(final.test$Survived, fitted.probabilities > 0.5)




