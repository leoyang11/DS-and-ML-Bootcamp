library(dplyr)
library(data.table)
library(ggplot2)
library(plotly)

## load dataset
batting<- fread('d:/Documents/leo/Rstudy/DS and ML Bootcamp/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Capstone Project/Batting.csv')

## explore
batting %>% head
str(batting)
batting$AB %>% head
batting$'2B' %>% head

## add new variable BA
batting$BA <- batting$H / batting$AB
tail(batting$BA,5)

## add new variables OBP, SLG
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP)
batting$'1B' <- batting$H - batting$'2B' - batting$'3B' - batting$HR
batting$SLG <- (batting$'1B' + 2*batting$'2B' + 3*batting$'3B' + 4*batting$HR) / batting$AB


## load dataset
sal<- fread('d:/Documents/leo/Rstudy/DS and ML Bootcamp/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Capstone Project/Salaries.csv')

## explore
sal %>% head()
summary(batting)
summary(sal)

## reassign batting data
batting <- subset(batting, batting$yearID >=1985)

## merge data
combo <- merge(x=batting, y=sal, by=c('playerID', 'yearID'))
combo %>% head
summary(combo)

## lost_players
lost_players <- combo %>% filter(playerID %in% c('damonjo01', 'giambja01', 'saenzol01'))

lost_players <- combo %>% 
  filter(playerID %in% c('damonjo01', 'giambja01', 'saenzol01')) %>%
  filter(yearID == 2001)

lost_players <- lost_players[,c('playerID', 'H', '2B', '3B', 'HR', 'OBP', 'SLG', 'BA','AB')]
lost_players

lost_players %>% summarise(sum(AB))    #1469
lost_players %>% summarise(mean(OBP))  #0.368
# salary cap  15000000


## combo2001
combo2001 <- combo %>% filter(yearID == 2001)
combo2001 %>% head()

combo2001 %>% ggplot(aes(x=salary, y=OBP)) + geom_point() + geom_text(aes(label=playerID))
ggplotly()

combo2001 %>% 
  filter(OBP > 0.4 & AB > 500) %>% 
  select('playerID','teamID.x','AB','OBP','salary')




