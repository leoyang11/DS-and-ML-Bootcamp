install.packages('rpart')
install.packages('rpart.plot')
install.packages('randomForest')
library(rpart)
library(rpart.plot)
library(randomForest)

str(kyphosis) 
head(kyphosis)

#### desicion tree modeling ####
tree <- rpart(Kyphosis ~ ., method = 'class', data = kyphosis)
printcp(tree)

#### drawing a tree ####
plot(tree, uniform = T, main = 'Kyphosis Tree')
text(tree, use.n = T, all = T)

#### rpart.plot 으로 그래프 그리기 ####
prp(tree)


#### Random Forest ####
rf.model <- randomForest(Kyphosis ~ ., data = kyphosis)
rf.model

#### predicted result를 볼 수 있다 ####
rf.model$predicted

#### tree 개수 확인 -> default 값은 500 ####
rf.model$ntree

# confusion matrix ####
rf.model$confusion


#### project - decision tree ####


#### load dataset ####
df <- as.data.frame(College)
df %>% names()
df %>% head()
str(df)

#### EDA ####
df %>% ggplot(aes(Room.Board, Grad.Rate)) +   geom_point(aes(color = Private))

df %>% ggplot(aes(F.Undergrad)) + geom_histogram(aes(fill = Private), color =
                                                   'black', bins = 50)

df %>% ggplot(aes(Grad.Rate)) + geom_histogram(aes(fill = Private), color =
                                                 'black', bins = 50)

df %>% filter(Grad.Rate > 100)
subset(df, Grad.Rate > 100)
df['Cazenovia College', 'Grad.Rate'] <- 100

#### Train Test Split ####
library(caTools)
split <- sample.split(df$Private, SplitRatio = 0.7)
str(split)

train <- subset(df, split == T)
test <- subset(df, split == F)

#### decision tree ####
dt <- rpart(Private ~ ., method = 'class', data = train)
printcp(dt)

#### decision tree visualizaton ####
plot(dt, uniform = T, main = 'Univ Tree')
text(dt, use.n = T, all = T)

#### decision tree predict ####
pred.dt <- predict(dt, newdata = test)
head(pred.dt)
pred.dt <- data.frame(pred.dt)

#### predict column add ####
pred.dt$pred <- ifelse(pred.dt$Yes > 0.5, "Y", "N")
test <- cbind(test, pred.dt)
table(test$Private, test$pred)
(59 + 153) / (59 + 5 + 16 + 153)

#### graph ####
prp(dt)


###############################################
### project - ramdomForest ####
###############################################

library(randomForest)
rf_model <- randomForest(Private ~ ., data = train, importance = T)
rf_model

#### confusion matrix ####
rf_model$confusion

#### model importance ####
rf_model$importance
varImpPlot(rf_model)

#### prediction ####
test$predict <- predict(rf_model, newdata = test)
table(test$Private, test$predict)
