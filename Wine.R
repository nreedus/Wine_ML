library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

## Load the wine.csv data
wine = read.csv("wine.csv")

## The wine data set has 7 variables and 25 observations
str(wine)

## The wine has a minimum price of 6.205 and a maximum price of 8.494 
summary(wine)

## model1 uses AGST (average growing season temperature as the independent variable)
model1 = lm(Price ~ AGST, data = wine)
 
## The Multiple R-squared = 0.435 and the adjusted R-squared = 0.4105 using only one independent variable
summary(model1)

## To see the residuals for each observations
model1$residuals

## The sum of squared errs SSE is 5.734875
SSE = sum(model1$residuals^2)

SSE

## The variable HarvestRain is added to model2
model2 = lm(Price ~ AGST + HarvestRain, data=wine)

## The Multiple R-squared = 0.7074 and the adjusted R-squared = 0.6808 using two independent variables
summary(model2)

## The sum of squared errs SSE is 2.970373
SSE = sum(model2$residuals^2)
SSE

## mode31 uses AGST (average growing season temperature), HarvestRain, WinterRain, Age and FrancePop as independent variables
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)

model3

## The multiple R-squared = 0.8294 and the adjusted R-squared = 0.7845 using five indendent variables
summary(model3)

## The sum of squared errs SSE is 1.732113
sse = sum(model3$residuals^2)

sse

## model4  removes the independent variable FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)

## The Multiple R-squared = 0.8286 and the	adjusted R-squared = 0.7943 
summary(model4)

## Computes the correlation between the independent variables WinterRain and Price
cor(wine$WinterRain, wine$Price)

cor(wine$Age, wine$FrancePop)

## Computes the correlation between all independent variables
cor(wine)

## model5 removes the independent variable Age
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)

summary(model5)

## Load the wine_test.csv data
wineTest = read.csv("wine_test.csv")

## The wine data set has 7 variables and 2 observations, wine from the years 1979 and 1980
str(wineTest)

## Model4 predicts the wine price of 6.76 for the year 1979 and 6.68 for the year 1980 
predictTest = predict(model4, newdata = wineTest)

predictTest

## Computes the SSE of the wineTest data
SSE = sum((wineTest$Price - predictTest)^2)

## Computes the SST of the wineTest data
SST = sum((wineTest$Price - mean(wine$Price))^2)

## Computes R-squared which is 0.7944278
1-SSE/SST
