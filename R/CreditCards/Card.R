library(dplyr)
library(tidyverse)
library(lattice)
library(ggplot2)
str(train)
test$installments[is.na(test$installments)] <- 0
test$days_of_week <- gsub(0 , "Mon", test$days_of_week)
test$days_of_week <- gsub(1 , "Tue", test$days_of_week)
test$days_of_week <- gsub(2 , "Wed", test$days_of_week)
test$days_of_week <- gsub(3 , "Thu", test$days_of_week)
test$days_of_week <- gsub(4 , "Fri", test$days_of_week)
test$days_of_week <- gsub(5 , "Sat", test$days_of_week)
test$days_of_week <- gsub(6 , "Sun", test$days_of_week)
test$hour <- substr(test$time, 1,2) 
test$year <- substr(test$date,1,4)
test$month <- substr(test$date,6,7)
test$season <- test$month
test$season <- gsub("01", "Winter", test$season)
test$season <- gsub("02", "Winter", test$season)
test$season <- gsub("03", "Spring", test$season)
test$season <- gsub("04", "Spring", test$season)
test$season <- gsub("05", "Spring", test$season)
test$season <- gsub("06", "Summer", test$season)
test$season <- gsub("07", "Summer", test$season)
test$season <- gsub("08", "Summer", test$season)
test$season <- gsub("09", "Autumn", test$season)
test$season<- gsub("10", "Autumn", test$season)
test$season <- gsub("11", "Autumn", test$season)
test$season<- gsub("12", "Winter", test$season)
test <- test[test$amount != 0,] 

test$holyday <- gsub('1', 'Holiyday', test$holyday)
test$holyday <- gsub(0, 'Non-Holiyday', test$holyday)


##########


test_no_installment <- test
test_no_installment <- test_no_installment[is.na(test_no_installment$installments) != FALSE,]

test_no_installment$hour <- substr(test_no_installment$time, 1,2) 


test_no_installment$year <- substr(test_no_installment$date,1,4)

test_no_installment$month <- substr(test_no_installment$date,6,7)
test_no_installment$season <- test_no_installment$month
summary(test_no_installment$season)
test_no_installment$season <- gsub("01", "Winter", test_no_installment$season)
test_no_installment$season <- gsub("02", "Winter", test_no_installment$season)
test_no_installment$season <- gsub("03", "Spring", test_no_installment$season)
test_no_installment$season <- gsub("04", "Spring", test_no_installment$season)
test_no_installment$season <- gsub("05", "Spring", test_no_installment$season)
test_no_installment$season <- gsub("06", "Summer", test_no_installment$season)
test_no_installment$season <- gsub("07", "Summer", test_no_installment$season)
test_no_installment$season <- gsub("08", "Summer", test_no_installment$season)
test_no_installment$season <- gsub("09", "Autumn", test_no_installment$season)
test_no_installment$season <- gsub("10", "Autumn", test_no_installment$season)
test_no_installment$season <- gsub("11", "Autumn", test_no_installment$season)
test_no_installment$season <- gsub("12", "Winter", test_no_installment$season)

test_no_installment %>%  group_by(season) %>%  summarize(amount.mean = mean(amount))
test_no_installment %>%  group_by(month) %>%  summarize(amount.mean = mean(amount))
library(ggplot2)
test_no_installment %>%  group_by(days_of_week) %>%  summarize(amount.mean = mean(amount))
boxplot(test$amount)
median(boxplot.stats(test$amount)$out)


