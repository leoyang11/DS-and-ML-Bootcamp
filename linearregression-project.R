
library(data.table)
library(dplyr)
library(ggplot2)

## read dataset
bike <- fread('bikeshare.csv')
bike %>% head()

## scatterplot of count vs. temp
bike %>% 
  ggplot(aes(temp, count)) + 
  geom_point(aes(color=temp),alpha = 0.3, position = position_jitter()) + 
  theme_bw()

## scatterplot of count vs. datetime
bike$datetime <- as.POSIXct(bike$datetime)
bike %>% 
  ggplot(aes(datetime, count)) + 
  geom_point(aes(color = temp), alpha=0.2) + 
  scale_color_continuous(low='#55D8CE',high='#FF6E2E') +
  theme_bw()

## correlation
cor(bike[,c('temp','count')])   # 변수 지정해서

num.cols <- sapply(bike, is.numeric)   # 수치형자료 전체
cor(bike[,num.cols])

## boxplot of season / count
bike %>% 
  ggplot(aes(as.factor(season), count)) + geom_boxplot(aes(color=as.factor(season)))

## add hour column in dataset
bike$hour <- hour(bike$datetime)

## scatterplot with various variables
bike %>% 
  ggplot(aes(hour, count)) +
  geom_jitter(aes(color = temp), alpha=0.3) + 
  scale_color_gradient(low = 'white', high = 'red') +
  theme_bw()

bike %>% 
  filter(workingday == 1) %>% 
  ggplot(aes(hour, count)) +
  geom_point(aes(color = temp, size=1.5), alpha=0.3, position = position_jitter(w=1, h=0)) + 
  scale_color_gradientn(colors = c('purple', 'navy', 'blue', 'green', 'yellow', 'orange', 'red')) +
  theme_bw()

## linear model (temp.model)
temp.model <- lm(count ~ temp, data = bike)
summary(temp.model)

## predict with temp.model
predict(temp.model, data.frame(temp=25))

## change hour to numeric value
bike$hour <- as.numeric(bike$hour)

## linear model (all var)
model <- lm(count ~ season + holiday + workingday + weather + temp + humidity + windspeed + hour, data = bike)
summary(model)








