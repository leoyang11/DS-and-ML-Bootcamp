adult <- read.csv("d:/Documents/leo/Rstudy/?”°?¼?•˜ë©? ë°°ìš°?Š” ?°?´?„°ê³¼í•™/my_work/adult.data", header=F, strip.white = T)
names(adult) <- c('age', 'workclass', 'fnlwgt', 'education',
                  'education_num', 'marital_status', 'occupation',
                  'relationship', 'race', 'sex',
                  'capital_gain', 'capital_loss',
                  'hours_per_week', 'native_country',
                  'wage')
adult %>% head()
adult %>% glimpse()


library(Amelia)
missmap(airquality, main = 'Missing Map', col = c('yellow', 'black'), legend = F)



airquality_2 <- airquality[complete.cases(airquality[,'Solar.R']),]

complete.cases(airquality[,'Solar.R'])


missmap(airquality_2, main = 'Missing Map', col = c('yellow', 'black'), legend = F)



airquality %>% head()

airquality_4 <- airquality
airquality_4$Ozone[is.na(airquality_4$Ozone)] <- mean(airquality_4$Ozone, na.rm = T)
mean(airquality_4$Ozone, na.rm = T)

airquality_5 <- airquality
airquality_5$Ozone[is.na(airquality_5$Ozone)] <- median(airquality_5$Ozone, na.rm = T)
median(airquality_5$Ozone, na.rm = T)


library(Amelia)
airquality_7 <- airquality

air_model <- lm(Ozone ~ Solar.R, data = airquality_7)
air_model
summary(air_model)



nodata <- airquality_6[is.na(airquality_6$Ozone),]
nodata
air_pred <- predict(air_model, nodata)
air_pred
nodata$Ozone <- air_pred



air_model2 <- lm(Ozone.x ~ Solar.R.x, data = airquality_7)
summary(air_model2)

install.packages('mice')
library(mice)


##
airquality_6 <- airquality
tempdata <- mice(airquality_6, maxit = 50, method = 'pmm')
summary(tempdata)

tempdata$imp$Ozone
tempdata$imp$Solar.R
completed.data <- complete(tempdata,1)

tempdata
completed.data

air_model2 <- lm(Ozone ~ Solar.R, data = completed.data)
air_model2
summary(air_model2)


#build predictive model
fit <- with(data = tempdata, exp = lm(Ozone ~ Solar.R)) 
fit

#combine results of all 5 models
combine <- pool(fit)
summary(combine)

######
library(Amelia)
airquality_7 <- airquality
tempdata2 <- amelia(x = airquality_7, m = 5)
summary(tempdata2)

tempdata2$imputations[[1]]
tempdata2$imputations[[2]]

tempdata2$imputations[[1]]$Ozone

write.amelia(tempdata2, file.stem = 'impute')

impute1 <- read.table('impute1.csv', header=T, sep=',', na.strings = NA, dec = ".", strip.white = T)

summary(impute1)

impute1

airquality

table(is.na(airquality))
sum(is.na(airquality))
colSums(is.na(airquality))



adult1 <- adult

colnames(adult1)[3] <- 'fnlweight'
head(adult1)

library(dplyr)
adult1 <- rename(adult1, 'country' = 'native_country', 'marital' = 'marital_status')
table(is.na(adult1))
colSums(is.na(adult1))

head(adult1)

adult1$age2 <- ifelse(adult1$age > 20, "old", "young")



age_fun <- function(age){
  z <- age
  for(i in 1:length(age))
    if(age > 50){
      z[i] <- 'old'
    }else if(age > 30){
      z[i] <- 'middle'
    }else{
      z[i] <- 'young'
    }
  return(z)
}


## fixed ages ê³„ì‚°?•˜ê¸? (?œ„?—?„œ ë§Œë“  impute_age ?•¨?ˆ˜ ?´?š©)
new_age <- age_fun(adult$age)

## Age ?ˆ˜? •?•˜ê¸?
adult$age3 <- new_age

head(adult,20)


str(adult)





adult4 <- adult

adult4 %>% head()
adult4 <- rename(adult4, edu = education_num)
adult4 <- rename(adult4, 'con' = 'country')



























