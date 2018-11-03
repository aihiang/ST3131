#Reading Data
data <- read.csv("C:/ST3131 Regression Analysis/Project/Life Expectancy/Life Expectancy Data.csv", header=TRUE)

library(dplyr)
library(tidyverse)
library(leaps)

#Cleaning Data (point 2.2 in report)
#Drop NA values --> 183 to 130 Observations left.
data <- data %>% drop_na()
attach(data)

#Convert categorial variables to numeric, 1 is developed countries, 0 is developed countries
data$Status = as.numeric(Status)
data$Status[Status == 2] = 0

#To exclude country and year from regression model
#Note the change of data name to data1, creating another copy to work on.
data1 = subset(data, select = -c(Country, Year))

#Basic Linear Model (2.4.1)
model1 <- lm(data1$Life.expectancy~.,data = data1)
summary(model1)

#Histogram of Life Expectancy
#hist(data1$Life.expectancy, breaks=seq(50,85,5),freq=FALSE, main="Histogram of Life Expectancy")
#curve(dnorm(x, mean=mean(data1$Life.expectancy), sd=sqrt(var(data1$Life.expectancy))), col="darkblue", lwd=2, add=TRUE, yaxt="n")

#Using Stepwise to Obtain Best Fit (2.4.2)
library(MASS)
null_model <- lm(data1$Life.expectancy~1,data=data1)
full_model <- lm(data1$Life.expectancy~.,data=data1) #. means all variables not already mentioned in the formula
step <- stepAIC(null_model,direction="both", scope=list(upper=full_model,lower=null_model), k=2, test="F")
step$anova
summary(step)

#After getting best fit.
y <- data1$Life.expectancy
x1 <- data1$Status
x2 <- data1$Income.composition.of.resources
x3 <- data1$Hepatitis.B
x4 <- data1$Adult.Mortality
x5 <- data1$HIV.AIDS
data2 = subset(data1,select=c(Life.expectancy, HIV.AIDS, Hepatitis.B,Income.composition.of.resources, Adult.Mortality))

#Residual Plot vs Fitted Values (3.1)
modelfull <- lm(y~x2+x3+x4+x5)
res <- modelfull$res
fv <- modelfull$fit
par(mfrow=c(1,1))
plot(fv,res,xlab="Fitted Values",ylab="Residuals",main="Plot of residuals against fitted values")
abline(h=0,lty=2)

#Residual Plot vs Independent Variables (3.1)
plot(x2,res,xlab="Income composition of resources",ylab="Residuals",main="Plot of residuals against Income composition of resources")
abline(h=0,lty=2)
plot(x3,res,xlab="Hepatitis B",ylab="Residuals",main="Plot of residuals against Hepatitis B")
abline(h=0,lty=2)
plot(x4,res,xlab="Adult Mortality",ylab="Residuals",main="Plot of residuals against Adult Mortality")
abline(h=0,lty=2)
plot(x5,res,xlab="HIV/AIDS",ylab="Residuals",main="Plot of residuals against HIV/AIDS")

#Testing Normality using Q-Q Plot (3.2)
par(mfrow=c(2,2))
plot(model1) 

#Testing Normality using Kolmogorov-Smirnov Test (3.2)
res <- model1$residuals
ks.test(res, "pnorm", mean(res), sd(res)) 

#Testing for Collinearity (3.3)
library(lattice)
splom(~data2[,1:5],groups = NULL,data = data2,axis.line.tck=0,axis.text.alpha=0)
cor(data2[1:5],use="complete.obs", method="kendall")

#Interaction Effect and Second Order Variable
library(ggplot2)
data2$HIV[data2$HIV.AIDS<0.2] = "low"
data2$HIV[data2$HIV.AIDS>=0.2] = "high"
data2$HIV
data2$HepB[data2$Hepatitis.B < 92] = "low"
data2$HepB[data2$Hepatitis.B >= 92] = "high"
data2$HepB

ggplot(data=data2, mapping = aes(x = Adult.Mortality,y = Life.expectancy, col = HepB))+
  geom_point()+
  geom_smooth(method = 'lm', fullrange="true")

ggplot(data=data2, mapping = aes(x = Adult.Mortality,y = Life.expectancy, col = HIV))+
  geom_point()+
  geom_smooth(method = 'lm',fullrange = "true")

#Improving Model By Adding Interaction Terms
#All possible interactions
x23<-x2*x3
x24<-x2*x4
x25<-x2*x5
x34<-x3*x4
x35<-x3*x5
x45<-x4*x5

m<-data.frame(y,x2,x3,x4,x5,x23,x24,x25,x34,x35,x45)
nullmodel = lm(y~1, data = m)
fullmodel = lm(y~., data = m)
step(nullmodel,data = m, scope = list(upper = fullmodel, lower = nullmodel), direction = "both",k = 2, test = "F")

#Best fit model: y ~ x2 + x3 + x25 + x34 + x35
best_model = lm(y~x2+x3+x25+x34+x35, data = m)
summary(best_model)
anova(best_model)


#Test for Coincidence
x12<-x1*x2
x13<-x1*x3
x125<-x1*x25
x134<-x1*x34
x135<-x1*x35
model3 <- lm(y~x2+x3+x25+x34+x35)
model4 <- lm(y~x2+x3+x25+x34+x35+x12+x13+x125+x134+x135)
anova(model3,model4)

#Residual Plot vs Fitted Values
res2 <- model3$res
fv2 <- model3$fit
par(mfrow=c(1,1))
plot(fv2,res,xlab="Fitted Values",ylab="Residuals",main="Plot of residuals against fitted values")
abline(h=0,lty=2)


#Residual Plot vs Independent Variables for model (2) (4.4)
#x1 <- Status 
#x2 <- Income.composition.of.resources 
#x3 <- Hepatitis.B
#x4 <- Adult.Mortality 
#x5 <- HIV.AIDS
plot(x25,res2,xlab="Income composition of resources",ylab="Residuals",main="Plot of residuals against HIV/AIDS and Income Composiiton of Resources")
abline(h=0,lty=2)
plot(x2,res2,xlab="Income composition of resources",ylab="Residuals",main="Plot of residuals against Income Composiiton of Resources")
abline(h=0,lty=2)
plot(x34,res2,xlab="Income composition of resources",ylab="Residuals",main="Plot of residuals against Hepatitis B and Adult Mortality")
abline(h=0,lty=2)
plot(x3,res2,xlab="Income composition of resources",ylab="Residuals",main="Plot of residuals against Hepatitis B")
abline(h=0,lty=2)
plot(x35,res2,xlab="Income composition of resources",ylab="Residuals",main="Plot of residuals against Hepatitis B and HIV/AIDS")
abline(h=0,lty=2)


#Testing Normality using Q-Q Plot (4.5)
par(mfrow=c(2,2))
plot(model3) 

#Testing Normality using Kolmogorov-Smirnov Test (4.5)
res3 <- model3$residuals
ks.test(res, "pnorm", mean(res3), sd(res3)) 

#Influential Points
library(car)
influence.measures(model3)
rstudent(model3)


#Plot showing Influential Points (7.12)
model3 <- lm(y~x2+x3+x25+x34+x35)
par(cex.axis = 0.5, cex.lab = 0.5, mar = c(5,5,2,2),mfrow = c(1,2))
plot(abs(dffits(model3)), ylim=c(0,1),type = "n", ylab = "DFFITS")
text(abs(dffits(model3)), labels = seq(1,n),cex=0.5)
abline(h=0.45, col = "red")






