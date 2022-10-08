#Project 1 Violent Crimes and Elections
rm(list = ls())
setwd("~/Dropbox/IRHonors_2021-2022/Luopeiwen Yi/Project 1")
load("/Users/Tina/Downloads/data_Colombia.RData")
data <- read.csv("~/Dropbox/IRHonors_2021-2022/Luopeiwen Yi/Assignment 3/data_Colombia.RData")
#number of observations 
str(data)
summary(data)
#summary statistics
summary(data$female_candidates)
summary(data$indigenous_candidates)
summary(data$afro_candidates)
summary(data$homicides)
#Histogram 
hist(data$female_candidates, xlab = "female candidates", ylab = "Frequency", main ="Histogram of female candidates in local election")
hist(data$indigenous_candidates, xlab = "indigenous candidates", ylab = "Frequency", main ="Histogram of indigenous Columbia candidates in local elections")
hist(data$afro_candidates, xlab = "afro candidates", ylab = "Frequency", main ="Histogram of afro Columbia candidates in local elections")
hist(data$homicides, xlab = "homocides", ylab = "Frequency", main ="Histogram of homocides committed in local municipality")
#Missing observations 
sum(is.na(data$female_candidates))
sum(is.na(data$indigenous_candidates))
sum(is.na(data$afro_candidates))
sum(is.na(data$homicides))
#Linear regressions for the outcome of female candidates without controls nor fixed effects
f1 <- lm(female_candidates ~ homicides, data = data)
summary(lm(female_candidates ~ homicides, data = data))
plot(y = data$female_candidates, x = data$homicides,
     pch = 16, frame = FALSE,
     xlab = "homicides in local municipality", ylab = " female candidates in local election", main = "homicides and female candidates",
     col = "#2E9FDF",
     abline(lm(data$female_candidates ~ data$homicides), col = "red", lwd = 2, lty = 1))
#Linear regressions for the outcome of indigenous candidates without controls nor fixed effects
i1 <- lm(indigenous_candidates ~ homicides, data = data)
summary(lm(indigenous_candidates ~ homicides, data = data))
plot(y = data$indigenous_candidates, x = data$homicides,
     pch = 16, frame = FALSE,
     xlab = "homicides in local municipality", ylab = "indigenous candidates in local election", main = "homocides and indigenous candidates",
     col = "#2E9FDF",
     abline(lm(data$indigenous_candidates ~ data$homicides), col = "red", lwd = 2, lty = 1))
#Linear regressions for the outcome of afro candidates without controls nor fixed effects
a1 <- lm(afro_candidates ~ homicides, data = data)
summary(lm(afro_candidates ~ homicides, data = data))
plot(y = data$afro_candidates, x = data$homicides,
     pch = 16, frame = FALSE,
     xlab = "homicides in local municipalities", ylab = "afro candidates in local election", main = "homocides and afro candidates",
     col = "#2E9FDF",
     abline(lm(data$afro_candidates ~ data$homicides), col = "red", lwd = 2, lty = 1))
#stargazer for f1,i1,a1 
library(stargazer)
stargazer(f1,i1,a1, digits = 2,omit = c("municipal_code", "municipality", "year"), type = "text")
#Linear regression for the outcome of female candidates with controls and fixed effects
f2 <- lm(female_candidates ~ homicides + high_conflict + rural + area + municipal_tax_contributions + distance_Bogota + altitude + factor(municipality) + factor(year), data = data)
summary(f2)
#Linear regression for the outcome of indigenous candidates with controls and fixed effects
i2 <- lm(indigenous_candidates ~ homicides + high_conflict + rural + area + municipal_tax_contributions + distance_Bogota + altitude + factor(municipality) + factor(year), data = data)
summary(i2)
#Linear regression for the outcome of afro candidates with controls and fixed effects
a2 <- lm(afro_candidates ~ homicides + high_conflict + rural + area + municipal_tax_contributions + distance_Bogota + altitude + factor(municipality) + factor(year), data = data)
summary(a2)
#stargazer for f2,i2,a2
stargazer(f2,i2,a2, digits = 2, omit = c("municipal_code", "municipality", "year"), type = "text")
#Regression controlling high conflicts 
female_highconflict <- lm(female_candidates ~ homicides*high_conflict, data=data)
summary(lm(female_candidates ~ homicides*high_conflict, data=data))
indigenous_highconflict <- lm(indigenous_candidates ~ homicides*high_conflict, data=data)
summary(lm(indigenous_candidates ~ homicides*high_conflict, data=data))
afro_highconflict <- lm(afro_candidates ~ homicides*high_conflict, data=data)
summary(lm(afro_candidates ~ homicides*high_conflict, data=data))
