## load packages
library(ggplot2)
library(ggthemes)
library(dplyr)

## load dataset
df <- read.csv('d:/Documents/leo/Rstudy/DS and ML Bootcamp/R-Course-HTML-Notes/student-mat.csv', sep = ';')
df %>% head()
summary(df)
any(is.na(df))

# 숫자형 컬럼만 따로 모으기
num.cols <- sapply(df, is.numeric)

# 숫자형 컬럼 간 correlation
cor.data <- cor(df[,num.cols])
cor.data

### visualization for EDA
install.packages('corrgram')
install.packages('corrplot')
library(corrgram)
library(corrplot)

corrplot(cor.data, method = 'color')
corrgram(df)  # corrgram은 dataframe을 직접 넣어도 동작함   
corrgram(df, order = T, lower.panel = panel.shade, 
         upper.panel = panel.pie, text.panel = panel.txt)

df %>% ggplot(aes(x=G3)) + geom_histogram(bins=20, alpha=0.5, fill='blue')


### Train과 Test 테스트 분리
install.packages('caTools')
library(caTools)

# Set a seed
set.seed(101)

# Split up sample
sample <- sample.split(df$G3, SplitRatio = 0.7)

train <- subset(df, sample == T)  # dplyr::  train <- df %>% filter(sample == T)
test <- subset(df, sample == F)

### TRAIN AND BUILD MODEL
model <- lm(G3 ~ ., data = train)
summary(model)

new.model <- step(model, direction = 'both')
summary(new.model)

## RUN MODEL
plot(model)


## residual 확인: 정규분포인가? 
res <- residuals(model)
res <- as.data.frame(res)
res
res %>% ggplot(aes(res)) + geom_histogram(fill='blue', alpha=0.5)

### PREDICTIONS
G3.predictions <- predict(model, test)
G3.predictions

results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted', 'actual')
results <- as.data.frame(results)
results

# predict 결과에 음수가 나온 걸 0으로 전환 (방법 1)
to_zero <- function(x){
  if(x < 0){
    return(0)
  }else{
    return(x)
  }
}
results$predicted <- sapply(results$predicted, to_zero)

# predict 결과에 음수가 나온 걸 0으로 전환 (방법 2)
results$predicted <- ifelse(results$predicted < 0, 0, results$predicted)

min(results$predicted)


## MSE (Mean Squared Error)
mse <- mean((results$actual - results$predicted)^2)
mse

## RMSE (Rooted Mean Squared Error)
rmse <- mse ^ 0.5
rmse

#########
SSE <- sum((results$predicted - results$actual)^2)
SST <- sum((mean(df$G3) - results$actual)^2)
R2 <- (1 - SSE/SST)
R2
