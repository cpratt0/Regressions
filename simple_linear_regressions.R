setwd("C:/Users/Pratt/Desktop/MBA/Classes/2019/Winter/Regressions/Data")

data<-read.csv("Scatterplots.csv")

#View data variables
names(data)

corrmatrix <- cor(data)
round(corrmatrix,2)

# Scatterplots
#Y1~X1
par(mfrow=c(1,2))
plot(data$X1,data$Y1,pch=20,xlab="X1",ylab="Y1")
abline(lm(Y1 ~ X1,data=data1))

#Y2~X2
par(mfrow=c(1,2))
plot(data$X2,data$Y2,pch=20,xlab="X2",ylab="Y2")
abline(lm(Y2 ~ X2,data=data1))

#Y3~X3
par(mfrow=c(1,2))
plot(data$X3,data$Y3,pch=20,xlab="X3",ylab="Y3")
abline(lm(Y3 ~ X3,data=data1))

#Y4~X4
par(mfrow=c(1,2))
plot(data$X4,data$Y4,pch=20,xlab="X4",ylab="Y4")
abline(lm(Y4 ~ X4,data=data1))

#Problem 1: Simulate from simple linear regression model
X_sample<-rnorm(100,-1,sd=sqrt(2.5)) #Simulated normal, with mean = -1 and variance = 2.5
error<-rnorm(100,0,sd=sqrt(3))
Y_sample <- 2.5 + 2*X_sample + error
par(mfrow=c(1,2))
plot(X_sample,Y_sample,pch=20,xlab="X",ylab="Y")
abline(lm(Y_sample ~ X_sample),col=2)

#Split sample into 2 subsets: 25 and 75
data_all <- data.frame(X_sample, Y_sample)
N = nrow(data_all)
data_all_1<-data_all[sample(1:N), ]
data_sub_1 = sample(1:N,size=as.integer(0.75*N),replace=F)
data_sub_75 = data_all_1[data_sub_1, ]
data_sub_25 = data_all_1[-data_sub_1,]

#Plot 3: entire sample, 25% sample, and 75% sample
plot(X_sample,Y_sample,pch=20,xlab="X",ylab="Y")
abline(lm(Y_sample ~ X_sample),col=2)
abline(lm(data_sub_75$Y_sample ~ data_sub_75$X_sample),col=3)
abline(lm(data_sub_25$Y_sample ~ data_sub_25$X_sample),col=4)
text(1, -4.5, "All", col = 2)
text(1, -5.25, "75% Sample", col = 3)
text(1, -6, "25% Sample", col = 4)
#Different samples --> different model fit
 
summary(data_all)

#Know 1.44 standard deviations around the mean capture 85% of data
#Var(Y) = Var(2.5 + 2X + epsilon)
#Var(Y) = 4*Var(X) + Var(epsilon) + 2*Cov(X,epsilon)
#Var(Y) = 4*2.5 + 3 + 2*0 = 13
#std_dev(Y) = sqrt(13)

#Plot 85% interval
plot(X_sample,Y_sample,pch=20,xlab="X",ylab="Y")
abline(h=0.5+1.44*sqrt(13),lty=2,col=3)
abline(h=0.5-1.44*sqrt(13),lty=2,col=4)
text(-3,6.25,"Upper Bound",col=3)
text(1,-5.25,"Lower Bound",col=4)

#Number of observations outside of 85% interval
sum(Y_sample > 0.5 + 1.44*sqrt(13)) + sum(Y_sample < 0.5 - 1.44*sqrt(13))

#Question 2
#Calculate S&P returns and VIX returns
lev <- read.csv("leverage.csv")
names(lev)
RetSPX<-diff(lev$SPX)/lev$SPX[-length(lev$SPX)]
RetVIX<-diff(lev$VIX)/lev$VIX[-length(lev$VIX)]
Ret<-data.frame(RetSPX, RetVIX)
View(Ret)

#Plot returns, calculate Beta coefficient
plot(RetVIX,RetSPX,pch=20,xlab="Daily VIX Returns",ylab="Daily SPX Returns")
lm(RetSPX~RetVIX)
cor(RetSPX,RetVIX)
sd(RetSPX)
sd(RetVIX)
b1 <- cor(RetSPX,RetVIX)*sd(RetSPX)/sd(RetVIX)
b1

#Plot Returns with best fit SLR line
plot(RetVIX,RetSPX,pch=20,xlab="Daily VIX Returns",ylab="Daily SPX Returns")
abline(lm(RetSPX~RetVIX),col=2)

model<-lm(RetSPX~RetVIX)
anova(model)
#sum(0.062381,0.023806)
summary(model)

#R-squared=0.7238.  Correlation is sqrt(R-squared)
sqrt(summary(model)$r.squared)

#Extract coefficients
coef(model)["(Intercept)"]
coef(model)["RetVIX"]

#Suppose we see VIX increase by 10%: what would happen to S&P returns
sum(coef(model)["(Intercept)"],0.1*coef(model)["RetVIX"])

#Find 90% predictive interval for S&P 500 Returns
mean_ret <- coef(model)["(Intercept)"]+mean(RetVIX)*coef(model)["RetVIX"]
lower_bound <- coef(model)["(Intercept)"]+mean(RetVIX)*coef(model)["RetVIX"] - qnorm(0.95,0,1)*0.025
upper_bound <- coef(model)["(Intercept)"]+mean(RetVIX)*coef(model)["RetVIX"] + qnorm(0.95,0,1)*0.025
print(lower_bound)
print(upper_bound)

#3: Vanguard Data

#Read in Data, and calculate Returns
van <- read.csv("vanguard.csv")
names(van)
len<-dim(van)[1]
RetSPX<-diff(log(van$SPX.INDEX))*52-van$TBILL[2:(len)]/100
#Return for 46 weekly individaul Vanguard ETFs
Retvan<-52*log(van[2:len,2:(dim(van)[2]-1)])-52*log(van[1:len-1,2:(dim(van)[2]-1)])-van$TBILL[2:(len)]/100     
names(Retvan)
View(Retvan)
#Perform QC on data
#write.csv(Retvan, "Log_Returns.csv")

#Apply CAPM to all ETFs
CAPM <- lm(as.matrix(Retvan) ~ RetSPX)
int <- CAPM$coefficients[1,]
slope <- CAPM$coefficients[2,]
write.csv(int, "Vanguard_Intercepts.csv")
write.csv(slope, "Vanguard_Slopes.csv")
print(CAPM)

#Plots Alphas vs. Beta
plot(CAPM$coeff[2,], CAPM$coeff[1,],
     ylab="alpha", xlab="beta", col=0)
text(x=CAPM$coeff[2,], y=CAPM$coeff[1,], labels=names(van)[2:(dim(van)[2]-2)], col=2)
#There’s a considerable concentration of ETF’s around beta = 1, and alpha = 0.  
#These tend to correspond with large-cap stocks.  Given that the S&P is comprised 500 large companies, 
#it is intuitive that ETF’s for large-cap stocks more in-step with the S&P 500.  
#Additionally, there is a group of ETF’s with beta=0, and low alpha (0 < alpha < 0.1).  
#These correspond with treasury/agency US bond ETF’s.  
#This is logical, given that treasury bills are a proxy for a risk-free asset.  

