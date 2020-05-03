#In this code, I do the following:

#1) Use data and regressions to find variables that have high explanatory power on crime (dataset on crimes in 1960)
#2) Predict of market returns using the VIX as an input

#1
setwd("C:/Users/Pratt/Desktop/MBA/Classes/2019/Winter/Regressions/Data")

data1<-read.csv("crime.csv")

summary(data1$CR)
summary(data1$W)
#View data variables
names(data1)


# Scatterplots
#i
#Visualize data
par(mfrow=c(1,3))
plot(data1$Ed,data1$CR,pch=20,xlab="Average Years of Education",ylab="Crime Rate")

plot(data1$LF,data1$CR,pch=20,xlab="Labor Force Participation",ylab="Crime Rate")

plot(data1$W,data1$CR,pch=20,xlab="Median Income",ylab="Crime Rate")

lm1<-lm(data1$CR~data1$Ed)
summary(lm1)

lm2<-lm(data1$CR~data1$LF)
summary(lm2)

lm3<-lm(data1$CR~data1$W)
summary(lm3)
#Univariate regressions demonstrate that median income (W) and
#education (ED) have statistically significant (positive) relationship on crime 


#iii
par(mfrow=c(1,2))
EDresid = resid(lm1)
plot(data1$Ed,EDresid,ylab="Residuals",xlab="Average Years of Education")
abline(0,0)

Wresid = resid(lm3)
plot(data1$W,Wresid,ylab="Residuals",xlab="Median Income")
abline(0,0)
#No clear pattern in residuals - demonstrates that my models may be viable.

#iv
CR1 <- data1$CR
W1 <- data1$W
W1.reg <- lm(CR1~W1)
summary(W1.reg)
Xf <- data.frame(W1=285)
n<-47
predict(W1.reg,newdata=Xf,se.fit=TRUE,interval="prediction",level=.95)$fit
#95% prediction interval demonstrates that, given a median income of $2,850 in
#1960, crime rate ranges from -28 offenses per million to 124 criminal offenses per million.
#This is not too helpful, considering a negative crime rate is not possible

#2
lev <- read.csv("leverage.csv")
names(lev)
n <- length(lev$SPX);
RetSPX <- log(lev$SPX[-1]/lev$SPX[-n])

n <- length(lev$VIX);
RetVIX <- log(lev$VIX[-1]/lev$VIX[-n])
summary(RetVIX)
Ret<-data.frame(RetSPX, RetVIX)

newx <- seq(-.80,.80,by=.01)
View(newx)
returns<-lm(RetSPX~RetVIX)

try <- data.frame(RetVIX=.1)
predict(returns,newdata=try,se.fit=TRUE,interval="prediction",level=0.90)
#Given a 10% increase in VIX, my linear regression predicts small positive returns to the S&P (@90% interval).

anova(returns)
summary(returns)
summary(names(returns))
#Some metrics to summarize the data/model.

plot(RetVIX,RetSPX,ylab="S&P Log Returns",xlab="VIX Log Returns")
abline(lm(RetSPX~RetVIX))
pred_tval<-predict(returns,newdata=data.frame(RetVIX=newx), interval="prediction", level=0.90)
lines(newx,pred_tval[,2], col="blue",lty=2)
lines(newx,pred_tval[,3], col="blue",lty=2)
#95% prediction interval for returns to S&P, using returns to VIX as an input

cor(RetSPX,RetVIX)
sd(RetSPX)
sd(RetVIX)
b1 <- cor(RetSPX,RetVIX)*sd(RetSPX)/sd(RetVIX)
b1
#Slightly negative correlation in S&P and VIX, as expected

plot(RetVIX,RetSPX,pch=20,xlab="Daily VIX Returns",ylab="Daily SPX Returns")
abline(lm(RetSPX~RetVIX),col=2)

model<-lm(RetSPX~RetVIX)
anova(model)
summary(model)

coef(model)["(Intercept)"]
coef(model)["RetVIX"]
sum(coef(model)["(Intercept)"],0.1*coef(model)["RetVIX"])
#Given 10% VIX, our model predicts slightly negative returns to S&P 500.

mean_ret <- coef(model)["(Intercept)"]+mean(RetVIX)*coef(model)["RetVIX"]
lower_bound <- coef(model)["(Intercept)"]+mean(RetVIX)*coef(model)["RetVIX"] - qnorm(0.95,0,1)*0.025
upper_bound <- coef(model)["(Intercept)"]+mean(RetVIX)*coef(model)["RetVIX"] + qnorm(0.95,0,1)*0.025

print(lower_bound)
print(upper_bound)
#Assuming mean value of VIX, 95% confidence interval yields returns between -4 and 4%.
#Calculations are interesting, but difficult to develop a trading strategy based solely on VIX.
