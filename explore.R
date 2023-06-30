##INSTALL the package "rstudioapi" to automate working directories.

##Figure out where this file is located, then set the working directory to this filepath
## This lets us open and close files using local paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


data = read.csv("Data.csv",header=T)
attach(data)

#Regression on the other ETFs
fit1 = lm(as.matrix(data$returns) ~ data$dbc + data$hyg + data$ief + data$spy)
summary(fit2)
plot(fit1$residuals)

plot(data$hyg, fit1$fitted.values)
plot(data$dbc, fit1$fitted.values)

#Regression on factors
fit2 = lm(as.matrix(data$returns) ~ data$united.states.ig.oas.all.sector + data$united.states.hy.oas.all.sector + data$united.states.cmt10y)
summary(fit2)
plot(fit2$residuals)

plot(data$united.states.ig.oas.all.sector, fit2$fitted.values)
plot(data$united.states.hy.oas.all.sector, fit2$fitted.values)
plot(data$united.states.cmt10y, fit2$fitted.values)

fit3 = lm(as.matrix(data$returns) ~ data$dbc + data$hyg + data$ief + data$spy
          + data$united.states.ig.oas.all.sector
          + data$united.states.hy.oas.all.sector
          + data$united.states.cmt10y)
summary(fit3)
library(faraway)
vif(fit3)
step(fit3)
anova(fit3)


fit4 = lm(as.matrix(data$returns) ~ data$dbc + data$hyg + data$ief + data$spy
          + data$united.states.ig.oas.all.sector
          + data$united.states.cmt10y)
summary(fit4)
library(faraway)
vif(fit4)
step(fit4)
anova(fit4)