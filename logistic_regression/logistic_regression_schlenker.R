## C. Schlenker - Mini Project: Logistic Regression
## Regression with binary outcomes
###################################

## Logistic regression
##################################

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:
setwd("M:/R-Springboard/Exercise7/logistic_regression")
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm


## Exercise: logistic regression
##################################

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.


## 1. GLM logistic regression to predict everwrk based on age_p and r_maritl

## check structure of everwrk
str(NH11$everwrk) #note that it has NAs
## subset NH11
NH11everwrk <- subset(NH11, select = c("everwrk", "age_p", "r_maritl"))
levels(NH11everwrk$everwrk) #check levels of everwrk
NH11everwrk$everwrk <- factor(NH11everwrk$everwrk, levels = c("2 No", "1 Yes")) #collapse to 2 levels
NH11everwrk$r_maritl <- droplevels(NH11everwrk$r_maritl)

# logistic regression model
everwrkMod <- glm(everwrk ~ age_p + r_maritl, data = NH11everwrk, family = "binomial")
#convert results into more interpretable odds vs log(odds)
everwrkOdds <- coef(summary(everwrkMod))
everwrkOdds[, "Estimate"] <- exp(coef(everwrkMod))
everwrkOdds

## 2. Predict probability of working for each level of marital status

## install effects package for prediction, already completed
## install.packages("effects")

## load effects
library(effects)

everwrkMarital <- as.data.frame(Effect("r_maritl", everwrkMod))
plot(everwrkMarital)
