library(dplyr)
library(ggplot2)
library(caTools)

detach("package:plyr", unload=TRUE)

## load dataset
adult <- read.csv('d:/Documents/leo/Rstudy/DS and ML Bootcamp/R-Course-HTML-Notes/adult_sal.csv')
adult %>% head()

adult <- adult %>% select(-X)
str(adult)
summary(adult)
levels(adult$income)

x <- model.matrix( ~ . -income, adult)

x
dim(x)
summary(x)
colnames(x)



## data cleaning
adult %>% group_by(type_employer) %>% count()
adult %>% group_by(type_employer) %>% summarise(n=n()) %>% arrange(n) %>% head(2)

## 조건에 따라 값 변경하기. 만약 stringAsFactor를  T로 가져왔다면, factor를 character로 우선 바꿔줘야 함
adult$type_employer <- as.character(adult$type_employer)

# 첫번째 방법
adult$type_employer <- ifelse(adult$type_employer == 'Never-worked' | adult$type_employer == 'Without-pay', 'Unemployed', adult$type_employer)

# 두번째 방법
adult$type_employer[adult$type_employer == 'Never-worked' | adult$type_employer == 'Without-pay'] <- 'Unemployed'


adult$type_employer <- ifelse(adult$type_employer == 'Local-gov' | adult$type_employer == 'State-gov', 'SL-gov', adult$type_employer)

adult$type_employer <- ifelse(adult$type_employer == 'Self-emp-inc' | adult$type_employer == 'Self-emp-not-inc', 'self-emp', adult$type_employer)

adult %>% group_by(type_employer) %>% count()

## marital

table(adult$marital)

adult$marital <- ifelse(adult$marital == 'Divorced' | adult$marital == 'Widowed' | adult$marital == 'Separated', 'Not-Married', adult$marital)

adult$marital <- ifelse(adult$marital == 'Married-AF-spouse' | adult$marital == 'Married-civ-spouse' | adult$marital == 'Married-spouse-absent', 'Married', adult$marital)

table(adult$marital)

## country
table(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)

table(adult$country)

## change char to factor
str(adult)
adult$type_employer <- factor(adult$type_employer)
adult$education <- factor(adult$education)
adult$marital <- factor(adult$marital)
adult$occupation <- factor(adult$occupation)
adult$relationship <- factor(adult$relationship)
adult$race <- factor(adult$race)
adult$sex <- factor(adult$sex)
adult$country <- factor(adult$country)
adult$income <- factor(adult$income)

## missing value check
library(Amelia)

adult[adult == '?'] <- NA
table(adult$type_employer)

missmap(adult, main = 'Missing Map', col = c('yellow', 'black'), legend = F)

adult <- na.omit(adult)
missmap(adult, main = 'Missing Map', col = c('yellow', 'black'), legend = F)


## EDA
adult %>% ggplot(aes(age)) + geom_bar(aes(fill = income), color='black') + theme_bw()
adult %>% ggplot(aes(age)) + geom_histogram(aes(fill = income), binwidth = 1,color='black') + theme_bw()

ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()


adult %>% ggplot(aes(hr_per_week)) + geom_histogram()

library(dplyr)
adult <- rename(adult, "region" = "country")  ## 변수명 바꾸기
adult %>% ggplot(aes(region)) + geom_bar(aes(fill = income)) + theme_bw()



## Logistic Regression
adult %>% head()
str(adult)

## train-test split
library(caTools)
split <- sample.split(adult$income, SplitRatio = 0.70)
ad.train <- subset(adult, split == T)
ad.test <- subset(adult, split == F)

ad.glm <- glm(formula = income ~ ., family = binomial(link = 'logit'), data = ad.train)
ad.glm <- glm(formula = income ~ ., family = binomial(logit), data = ad.train)  #으로 써도 됨 

summary(ad.glm)

new.ad.glm <- step(ad.glm)
summary(new.ad.glm)

## predict
ad.test$predicted.income <- predict(new.ad.glm, newdata = ad.test, type = 'response')
table(ad.test$income, ad.test$predicted.income > 0.5)

library(ROCR)
p <- predict(new.ad.glm, newdata = ad.test, type = 'response')
pr <- prediction(p, ad.test$income)
prf <- performance(pr, measure = 'tpr', x.measure = 'fpr')

plot(prf)
abline(0,1)

performance(pr, 'auc')@y.values[[1]]

head(p)
pr
prf

ad.test %>% head()
