#This code addresses the following:
#1. Review of OJ sales data, and constructing a model that best predicts sales (Model Selection using In-Sample Techniques)
#2. Estimating the relationship between female labor supply and birth (e.g., how female labor
#supply changes with an additional birth) - Using Instrumental Variables
#3. Predicting hourly bike rentals based on weather data, and using out-of-sample techniques to
#find the best model

setwd("C:/Users/Pratt/Desktop/MBA/Classes/2019/Winter/Regressions/Data")

#########1.  Review of OJ sales data, and constructing a model that best predicts sales

OJ<-read.csv("OJ.csv")

#Let's start with exploring the data

names(OJ)

par(mfrow=c(1,1))
plot(log(OJ$minuteprice),log(OJ$minutevol),pch=20,main="Minute Maid Volume & Minute Maid Price",xlab="log(Minute Maid Price)",ylab="log(Minute Maid Volume)")
#This plot shows that as the price of Minute Maid increase, the quantity of sales generally decreases.

minad <- ifelse(OJ$minutead == 1,"MM Advertised","MM Not Advertised")
par(mfrow=c(1,2))
boxplot(log(OJ$minutevol)~minad,ylab="log(Minute Maid Volume)",main="Minute Maid Volume")
boxplot(log(OJ$minuteprice)~minad,ylab="log(Minute Maid Price)",main="Minute Maid Price")

minad <- ifelse(OJ$minutead == 1,"MM Advertised","MM Not Advertised")
troad <- ifelse(OJ$tropicad == 1,"TC Advertised","TC Not Advertised")
dmoad <- ifelse(OJ$dmnckad == 1,"DK Advertised","DK Not Advertised")

par(mfrow=c(1,3))
boxplot(log(OJ$minutevol)~minad,ylab="log(Minute Maid Volume)",main="Minute Maid Volume")
boxplot(log(OJ$minutevol)~troad,ylab="log(Minute Maid Volume)",main="Minute Maid Volume")
boxplot(log(OJ$minutevol)~dmoad,ylab="log(Minute Maid Volume)",main="Minute Maid Volume")

#This plot shows that the price of all three brands is minimally affected by whether 
#or not the brand is advertised.  

#View(dmoad)

par(mfrow=c(1,3))
plot(log(OJ$minuteprice), log(OJ$minutevol),main="Minute Maid Volume & Minute Maid Price",xlab="log(Minute Maid Price)",ylab="log(Minute Miad Volume")
plot(log(OJ$tropicprice), log(OJ$minutevol),main="Minute Maid Volume & Tropicana Price",xlab="log(Tropicana Price)",ylab="log(Minute Miad Volume")
plot(log(OJ$dmnckprice), log(OJ$minutevol),main="Minute Maid Volume & Dominick's Price",xlab="log(Dominick's Price)",ylab="log(Minute Miad Volume")
#There appears to be the strongest negative relationship between Minute Maid Volume and Minute Paid Price

mean_minprice <- tapply(favoriteCount [source=="Android"], linear[source=="Android"], mean) 
mean_troprice <- tapply(favoriteCount [source=="iPhone"], linear[source=="iPhone"], mean)

log_data <- data.frame(log_minutevol =  log(OJ$minutevol),
log_tropicprice =  log(OJ$tropicprice),
log_minuteprice =  log(OJ$minuteprice),
log_dmnckprice =  log(OJ$dmnckprice))

#View(log_data)
pairs(log_data[,1:4],pch=20,col=1)
cor(log_data)

#The pairs plots and correlations demonstrates that there is low correlation between the 
#price of Tropicana and Minute Maid sales, and between the price Dominick’s and Minute Maid 
#sales  However, as expected, there is -0.48 correlation between the Minute Maid price and Minute Maid sales. 
#As expected, there is also a positive correlation between the prices of the three brands.

#We'll begin constructing more and more complex models to predict the volume of Minute Maid Sales

reg1 <- lm(log(OJ$minutevol)~log(OJ$minuteprice))
summary(reg1)

reg2 <- lm(log(OJ$minutevol)~log(OJ$minuteprice) + OJ$minutead)
summary(reg2)

reg3 <- lm(log(OJ$minutevol)~log(OJ$minuteprice) + OJ$minutead + log(OJ$dmnckprice)*log(OJ$tropicprice) -log(OJ$dmnckprice) - log(OJ$tropicprice))
summary(reg3)

reg4 <- lm(log(OJ$minutevol)~log(OJ$minuteprice) + OJ$minutead + log(OJ$dmnckprice)*log(OJ$tropicprice) + log(OJ$minuteprice)*log(OJ$dmnckprice)*log(OJ$tropicprice)
-log(OJ$dmnckprice) - log(OJ$tropicprice)
-log(OJ$dmnckprice) - log(OJ$tropicprice) - log(OJ$minuteprice)
-log(OJ$dmnckprice):log(OJ$tropicprice) 
-log(OJ$minuteprice):log(OJ$dmnckprice))
summary(reg4)

reg5 <- lm(log(OJ$minutevol)~log(OJ$minuteprice) + OJ$minutead + log(OJ$dmnckprice)*log(OJ$tropicprice) + log(OJ$minuteprice)*log(OJ$dmnckprice)*log(OJ$tropicprice)
+OJ$tropicad
-log(OJ$dmnckprice) - log(OJ$tropicprice)
-log(OJ$dmnckprice) - log(OJ$tropicprice) - log(OJ$minuteprice)
-log(OJ$dmnckprice):log(OJ$tropicprice) 
-log(OJ$minuteprice):log(OJ$dmnckprice))
summary(reg5)

reg6 <- lm(log(OJ$minutevol)~log(OJ$minuteprice) + OJ$minutead + log(OJ$dmnckprice)*log(OJ$tropicprice) + log(OJ$minuteprice)*log(OJ$dmnckprice)*log(OJ$tropicprice)
+OJ$tropicad+OJ$dmnckad
-log(OJ$dmnckprice) - log(OJ$tropicprice)
-log(OJ$dmnckprice) - log(OJ$tropicprice) - log(OJ$minuteprice)
-log(OJ$dmnckprice):log(OJ$tropicprice) 
-log(OJ$minuteprice):log(OJ$dmnckprice))
summary(reg6)

#Now, let's find the best model, of the six models we constructed

par(mfrow=c(2,3))
plot(reg1$fitted,rstudent(reg1), main="Studentized Residuals: Regression 1",col=3, pch=20, cex=1.5)
abline(a=0,b=0)
plot(reg2$fitted,rstudent(reg2), main="Studentized Residuals: Regression 2",col=3, pch=20, cex=1.5)
abline(a=0,b=0)
plot(reg3$fitted,rstudent(reg3), main="Studentized Residuals: Regression 3",col=3, pch=20, cex=1.5)
abline(a=0,b=0)
plot(reg4$fitted,rstudent(reg4), main="Studentized Residuals: Regression 4",col=3, pch=20, cex=1.5)
abline(a=0,b=0)
plot(reg5$fitted,rstudent(reg5), main="Studentized Residuals: Regression 5",col=3, pch=20, cex=1.5)
abline(a=0,b=0)
plot(reg6$fitted,rstudent(reg6), main="Studentized Residuals: Regression 6",col=3, pch=20, cex=1.5)
abline(a=0,b=0)

#Each regression has studentized residuals greater than 4, demonstrating that a linear regression may not be suitable.  
#Moreover, the studentized residuals for all regressions appear to have non-constant variance (greater variance in the 14.2 to 14.7 range, 
#smaller variance in the 13.0 to 14.0 range).  

par(mfrow=c(2,3))
qqnorm(rstudent(reg1),col=4,main="QQ-Plot: Regression 1")
abline(a=0,b=1)
qqnorm(rstudent(reg2),col=4,main="QQ-Plot: Regression 2")
abline(a=0,b=1)
qqnorm(rstudent(reg3),col=4,main="QQ-Plot: Regression 3")
abline(a=0,b=1)
qqnorm(rstudent(reg4),col=4,main="QQ-Plot: Regression 4")
abline(a=0,b=1)
qqnorm(rstudent(reg5),col=4,main="QQ-Plot: Regression 5")
abline(a=0,b=1)
qqnorm(rstudent(reg6),col=4,main="QQ-Plot: Regression 6")
abline(a=0,b=1)

#Regression 1 appears to have the residuals that least closely follow a normal distribution.

n <- nrow(OJ)

print(BIC <- c(reg1=extractAIC(reg1, k=log(n))[2],
               reg2=extractAIC(reg2, k=log(n))[2],
		   reg3=extractAIC(reg3, k=log(n))[2],
               reg4=extractAIC(reg4, k=log(n))[2],
               reg5=extractAIC(reg5, k=log(n))[2],
               reg6=extractAIC(reg6, k=log(n))[2]))

#According to the BIC criterion, regression 3 (regressing log(Minute Maid Volume) on log(Minute Maid Price), 
#Minute Maid Advertisements, and the interaction term between log(Tropicana price) and log(Dominick’s price)) 
#has the lowest BIC value and the highest probability of being the best model.

#This is a good first pass, but we ultimately must construct a more complex econometric model to predict
#the demand of MM sales

###########2. Estimating the relationship between female labor supply and birth (e.g., how female labor supply changes with an additional birth)

fert<-read.csv("fertility.csv")
View(fert)

#Part i

names(fert)

reg<-lm(fert$weeksm1~fert$morekids)

summary(reg)

#On average, women with more than two kids (indicated by "morekids" = 1) do appear to work less than women with two children. 
#The regression output below illustrates that women with more than two children work about 6 weeks less per year than women with less than two children.

#However, R-squared value of the model is 0.01762. That means that the regression explains 1.762% of the hours worked. This shows a poor relation between the variables.
#Additionally, “morekids” may not be appropriate for causality purposes because there may be fundamental differences between women who have two or more kids, 
#and women that have fewer than two children.  For example, women who are more career-oriented may opt to have fewer than two children, thereby allowing them to 
#work more weeks per year on average.  Under this construct, having two or more children does not cause women to work fewer weeks per year on average.  

#Next, we'll seek to address if couples whose first two children are the same sex more likely to have a third child

reg<-lm(fert$morekids~fert$samesex)

summary(reg)

#By regressing “morekids” on “samesex,” we see that couples whose first two children are of the same sex are more likely to have a third child.  
#The effect is not large (coefficient on “samesex” is only 0.06682).  It is statistically significant based on the t-test and the F-value (143.1).

regiv<-lm(fert$weeksm1~fert$samesex)

summary(regiv)

#The samesex variable is a valid instrument for the instrumental regression of weeksworked on morekids because it is somewhat correlated (has relevance) 
#with the independent variable, morekids (i.e., couples who have two children of the same sex may want to have another child). 
#Additionally, the instrument appears to be exogenous because samesex has little correlation with the residual of the linear model (weeks worked on more kids). 
#When weeksworked is regressed on samesex, samesex is insignificant. The entire model has an R-square close to zero.

#Part v

install.packages('sem')
require(sem)

summary(tsls(fert$weeksm1~fert$morekids,~fert$samesex))

#The fertility effect on weeksworked, when including the instrumental variable samesex, is the same as it was in the original model, at 6 weeks less with 2+ children. 
#Critically, the p-value of 0.11 demonstrates that this effect is NOT statistically significant.

############3. Predicting hourly bike rentals based on weather data, and using out-of-sample techniques to
############   find the best model

bike <- read.csv("bikeSharing.csv")

#First, let's visualize the data

par(mfrow=c(2,3))
plot(bike$temp,bike$atemp,xlab="Temp (Celsius)",ylab="'Feels Like' Temp (Celsius)")

diff = bike$temp - bike$atemp

bike2 <- bike[ which(bike$temp - bike$atemp < 13), ]

plot(bike2$temp,bike2$atemp,xlab="Temp (Celsius)",ylab="'Feels Like' Temp (Celsius)")
plot(bike2$humidity,bike2$temp)

bike3 <- bike2[ which(bike2$humidity > 5), ]

plot(bike3$humidity,bike3$temp)
plot(bike3$windspeed,bike3$temp)
plot(bike3$weather,bike3$temp)

#There are two sets of points that are clearly outliers in the temperature data.  
#First, there is one day (August 17, 2012) where the “feels like” temperature is 12.12 degrees, 
#while the temperature that day varies from 25 to 35 degrees.  For most other days, we see that the 
#actual temperature and the “feels like” temperature are much more similar. We would need to check 
#with the data collector to determine if there was an error in the atemp recording these days, or 
#if they had some other factor that led to the difference.  For our model, we will remove these points.

View(bike3)

#For model validation purposes, we'll split the data in to training and testing samples

index <- sample(1:nrow(bike3),7150)

training <- bike3[index,]
validation <- bike3[-index,]

#OK - training and validation were successfully compiled

#View(training)
#View(validation)

#Next, we'll create a series of dummy regressors, and use forward stepwise regression to
#construct a model that determines the most important hours of the day for predicting bike rental demand.

install.packages("tidyr")
require("tidyr")
training1 <- separate(training, datetime, into = c("date", "time"), sep = " ")

View(training1)

training2 = subset(training1,select = -c(date))

#View(training2)

XY <- training2[,-(10:11)]

names(XY)

null_count <- lm(count ~ 1, data=XY)
full_count <- lm(count ~ ., data=XY)
fwd_count <- step(null_count,scope=formula(full_count),direction="forward",k=log(7150)) 

summary(lm(XY$count~XY$time + XY$atemp + XY$weather + XY$season + XY$humidity + XY$temp))

names(training2)

XY_casual <- training2[,-(11:12)]
names(XY_casual)

null0 <- lm(casual ~ 1, data=XY_casual)
full0 <- lm(casual ~ ., data=XY_casual)
fwd0 <- step(null0,scope=formula(full0),direction="forward",k=log(7150))

summary(lm(XY_casual$casual~XY_casual$time + XY_casual$atemp + XY_casual$workingday + XY_casual$humidity + 
XY_casual$windspeed + XY_casual$holiday + XY_casual$weather))

#For casual users, regression demonstrates that hours 14:00 - 15:00 and 17:00 are the most important hours of the day for predicting bike rental demand.  

names(training2)
XY_registered0 <- training2[,-(10)]
names(XY_registered0)
XY_registered <- XY_registered0[,-(11)]
names(XY_registered)

names(XY_registered)

null1 <- lm(registered ~ 1, data=XY_registered)
full1 <- lm(registered ~ ., data=XY_registered)
fwd1 <- step(null1,scope=formula(full1),direction="forward",k=log(7150))

summary(lm(XY_registered$registered~XY_registered$time + XY_registered$atemp + XY_registered$workingday + XY_registered$season + 
XY_registered$humidity + XY_registered$weather + XY_registered$temp))

#For registered users, regression demonstrates that hours 17:00 - 19:00 are the most important hours of the day for predicting bike rental demand.  
 
#Return to aggregated equation, analyze season

XY_spring <- XY[ which(XY$season ==1), ]
XY_summer <- XY[ which(XY$season ==2), ]
XY_fall   <- XY[ which(XY$season ==3), ]
XY_winter <- XY[ which(XY$season ==4), ]

summary(lm(XY_spring$count~XY_spring$time + XY_spring$atemp + XY_spring$weather + XY_spring$season + XY_spring$humidity + XY_spring$windspeed + XY_spring$temp))
summary(lm(XY_summer$count~XY_summer$time + XY_summer$atemp + XY_summer$weather + XY_summer$season + XY_summer$humidity + XY_summer$windspeed + XY_summer$temp))
summary(lm(XY_fall$count~XY_fall$time + XY_fall$atemp + XY_fall$weather + XY_fall$season + XY_fall$humidity + XY_fall$windspeed + XY_fall$temp))
summary(lm(XY_winter$count~XY_winter$time + XY_winter$atemp + XY_winter$weather + XY_winter$season + XY_winter$humidity + XY_winter$windspeed + XY_winter$temp))

#Based on regression results, important hours do not vary with season.  

#Now, we'll make a more logical group of time dummies (e.g., "Night," "Commuting time"), and re-run our regression from above to inspect relationships
#between bike ridership and time

partofday <- ifelse(XY$time == "7:00" | XY$time == "8:00" | XY$time == "9:00" | XY$time == "15:00" | XY$time == "16:00" | XY$time == "17:00" | XY$time == "18:00" ,"Commute",
ifelse(XY$time == "10:00" | XY$time == "11:00" | XY$time == "12:00" | XY$time == "13:00" | XY$time == "14:00","Off-peak morning","Night"))

#View(partofday)
#Quality check - variable "partofday" is being assigned successfully.

names(XY)

null_hour <- lm(count ~ ., data=XY)
full_hour <- lm(count ~ . + .^2, data=XY)
fwd_hour  <- step(null_hour,scope=formula(full_hour),direction="forward",k=log(7150))

summary(lm(count ~ time + season + holiday + workingday + weather + temp + 
    atemp + humidity + windspeed + time*workingday - time - workingday + time*atemp - time - atemp + 
    temp*atemp - temp - atemp + season*humidity - season - humidity + weather*humidity - weather - humidity + workingday*temp - workingday - temp + 
    weather*atemp - weather - atemp + weather*windspeed - weather - windspeed + workingday*windspeed - workingday - windspeed +  
    workingday*temp - workingday - temp,data=XY))

XY$partofday <- partofday 
View(XY)
names(XY)

XY_logic <- XY[,-(1)]
names(XY_logic)

null_partofday <- lm(count ~ ., data=XY_logic)
full_partofday <- lm(count ~ . + .^2, data=XY_logic)
fwd_partofday <- step(null_partofday,scope=formula(full_partofday),direction="forward",k=log(7150))

summary(lm(count ~season + holiday + workingday + weather + temp + atemp + 
    humidity + windspeed + partofday + workingday:partofday + 
    season:partofday + atemp:humidity + temp:atemp + workingday:temp + 
    workingday:humidity + season:humidity + humidity:partofday + 
    weather:partofday + weather:humidity + workingday:weather + 
    atemp:partofday + temp:partofday,data = XY_logic))

#The grouping demonstrates, interestingly, a strong relationship between off-peak morning hours and bike ridership.  

####Now, using all of the models we analyzed above, we will calculate mean squared errors for the out-of-sample data.

names(validation)
View(validation)
names(null_count)

validation1 <- separate(validation, datetime, into = c("date", "time"), sep = " ")

validation2 = subset(validation1,select = -c(date))

names(validation2)

XY_validation <- validation2[,-(10:11)]
nrow(XY_validation)

#View(training2)
names(XY_validation)
View(XY_validation)

errorNull1 <- predict(null_count,newdata=XY_validation)-XY_validation$count
errorBIC1 <- predict(fwd_count,newdata=XY_validation)-XY_validation$count
errorFull1 <- predict(full_count,newdata=XY_validation)-XY_validation$count

partofday <- ifelse(XY_validation$time == "7:00" | XY_validation$time == "8:00" | XY_validation$time == "9:00" | XY_validation$time == "15:00" | XY_validation$time == "16:00" | XY_validation$time == "17:00" | XY_validation$time == "18:00" ,"Commute",
ifelse(XY_validation$time == "10:00" | XY_validation$time == "11:00" | XY_validation$time == "12:00" | XY_validation$time == "13:00" | XY_validation$time == "14:00","Off-peak morning","Night"))


names(XY_validation)

errorNullinthour <- predict(null_hour,newdata=XY_validation)-XY_validation$count
errorBICinthour <- predict(fwd_hour,newdata=XY_validation)-XY_validation$count
errorFullinthour <- predict(full_hour,newdata=XY_validation)-XY_validation$count

XY_validation$partofday <- partofday
XY_validation <- validation [,-(1)]
names(XY_validation)

errorNullpartofday <- predict(null_partofday,newdata=XY_validation)-XY_validation$count
errorBICpartofday <- predict(fwd_partofday,newdata=XY_validation)-XY_validation$count
errorFullpartofday <- predict(full_partofday,newdata=XY_validation)-XY_validation$count

mean(errorNull1^2)
mean(errorBIC1^2)
mean(errorFull1^2)

mean(errorNullinthour^2)
mean(errorBICinthour^2)
mean(errorFullinthour^2)

mean(errorNullpartofday^2)
mean(errorBICpartofday^2)
mean(errorFullpartofday ^2)

#When using the validation sample, the full model with all interaction terms, using hours as dummy variables instead of 
#“partofday” as a dummy variable has the lowest mean squared error of the 9 models interrogated.  

#Having an instance where a “full” model has the best fit is surprising.  
#For a more detailed analysis, I would perform a simulation of randomly generated “training” and “validation” datasets, 
#and then calculate MSE.  This would give a more appropriate distribution as to which model fits the validation dataset most appropriately.



