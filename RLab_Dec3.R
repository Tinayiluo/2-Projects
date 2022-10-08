# R SCRIPT FOR LAB 5: IR SENIOR SEMINAR 
# DECEMBER 3RD 2020

#Always start off any script with this command. It clears the global environment. 
rm(list = ls())

library(dplyr) 
#install.package("ggplot2")
#install.package("stargazer")
library(ggplot2)
library(stargazer)

#Set the working directory
setwd("~/Dropbox/IRHonors_2021-2022/RLab/12-03-2021")

##### Running a model with fixed effects ######

#Loading the panel data
data <- read.csv("~/Dropbox/IRHonors_2021-2022/RLab/12-03-2021/Panel101.csv")
data <- data[,-1]

table(data$country)
table(data$year)


### Data visualizations with ggplot 
#A few great sources: 

#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#https://www.r-graph-gallery.com/ggplot2-package.html


#Plots by country - using ggplot
data %>% 
  ggplot(aes(year,y, col = country)) +
  geom_line()

#Plots by country and year - using ggplot
data %>% 
  ggplot( aes(year, y)) +
  geom_line() +
  facet_wrap(~country)



#Fixed effects model
#Suppose x1 is the main explanatory variable of interest

#Here we have the naive regression, no controls
m1 <- lm(y ~ x1 , data = data)

#Adding two controls, x2 and x3
m2 <- lm(y ~ x1 + x2 + x3, data = data)

#Controls and country fixed effects 
m3 <- lm(y ~ x1 + x2 + x3 + factor(country), data = data)

#...
m4 <- lm(y ~ x1 + factor(country) , data = data)

m5 <- lm(y ~ x1 + factor(country) + factor(year), data = data)

m6 <- lm(y ~ x1 + x2 + x3 + factor(country) + factor(year), data = data)


stargazer(m1, m2,m3,m4,m5, m6, digits = 2, omit = c("country", "year"), type = "text")


rm(m1,m2,m3,m4,m5,m6)

####### Running a difference in difference model ######
#First you need to define pre-post year 
#You can think of it as the year after which treated units become treated 
#For this example, lets suppose the treatment year is 1993
#We create a dummy indicating any year including and post-1993, and any year after
data$time <- ifelse(data$year > 1993,1,0)

#Let's check now to ensure the coding is accurate 
table(data$time, data$year)

#Now, lets suppose treated observations are those where x1 are those greater than average, and control are those lower than average 
data$treatment <- ifelse(data$x1 > mean(data$x1, na.rm = T),1,0)

table(data$treatment)

#Finally lets suppose there are a few different outomes of interest, lets rename some of the variables for the purposes of illustration
colnames(data)[4] <- "y1"
colnames(data)[7] <- "y2"
colnames(data)[8] <- "y3"


#The basic DiD regression: interacting the treatment with post, including year and country fixed effects
did1 <- lm(y1 ~ treatment*time + factor(country) + factor(year), data = data)
did2 <- lm(y2 ~ treatment*time + factor(country) + factor(year), data = data)
did3 <- lm(y3 ~ treatment*time + factor(country) + factor(year), data = data)

stargazer(did1,did2,did3, digits = 2,omit = c("country", "year"), type = "text")

#The interaction term is the estimate of interest here. 



#A first look at examining the main assumption: parallel trends (binary treated and controls)
#How do we do this? We look to see that, prior to the treatment year, trends are parrallel (often referred to as checking the pre-trends)
#First need to create a variable that is a factor of the treatment. 
data$Treat <-  as.factor(data$treatment)
#only 27 NA's 

ggplot(filter(data, is.na(Treat) == F) , aes(year, y1, color =Treat)) + 
  stat_summary(fun = "mean", geom = "line") + 
  stat_summary(fun = "mean", geom = "point") +
  geom_vline(xintercept = 1994) +
  theme_minimal() + scale_x_continuous(breaks=seq(1990, 1999, 1))  + 
  # ylim(0,100) + 
  labs(title = "Examining pre-trends",
       x = "Year",
       y = "Outcome (y1)") 


ggplot(filter(data, is.na(Treat) == F) , aes(year, y2, color =Treat)) + 
  stat_summary(fun = "mean", geom = "line") + 
  stat_summary(fun = "mean", geom = "point") +
  geom_vline(xintercept = 1994) +
  theme_minimal() + scale_x_continuous(breaks=seq(1990, 1999, 1))  + 
  # ylim(0,100) + 
  labs(title = "Examining pre-trends",
       x = "Year",
       y = "Outcome (y2)") 


ggplot(filter(data, is.na(Treat) == F) , aes(year, y3, color =Treat)) + 
  stat_summary(fun = "mean", geom = "line") + 
  stat_summary(fun = "mean", geom = "point") +
  geom_vline(xintercept = 1994) +
  theme_minimal() + scale_x_continuous(breaks=seq(1990, 1999, 1))  + 
  # ylim(0,100) + 
  labs(title = "Examining pre-trends",
       x = "Year",
       y = "Outcome (y3)") 


