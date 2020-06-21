###Regression Methods Explored with this code:

#Model Fit/Selection with Fama-French Factor Model

#Time Series Analysis - using auto-correlation structure and out-of-sample 
#estimates to find most suitable model for prediction

#Multivariate Logistic Regression, Model Selection, and Model Interpretation

###Specific Data Examined:
#Determing best (empirical) Fama-French model to fit Apple stock returns

#Analyzing if UK's law to start wearing seat belts (effective 2/1/1983) 
#was effective in increasing driver safety

#Telephone company data; building a model to predict customer churn (i.e.,
#leaving the company in favor of a competitor)

##################Apple stock, and Fama-French model##################

mar <- read.csv("market.csv")

names(mar)
#View(mar)

ARETminusRF <- mar$AAPL - mar$RF
TRETminusRF <- mar$TSLA - mar$RF

MKTminusRF <- mar$MKTminusRF

SMB <- mar$SMB
HML <- mar$HML
RMW <- mar$RMW
CMA <- mar$CMA
MOM <- mar$MOM

areg1 <- lm(ARETminusRF~MKTminusRF)
areg2 <- lm(ARETminusRF~MKTminusRF + SMB + HML + RMW + CMA)
areg3 <- lm(ARETminusRF~MKTminusRF + SMB + HML + RMW + CMA + MOM)

#Investigate Apple Regressions
summary(areg1)
summary(areg2)
summary(areg3)

#Investigate Studentized Residuals
par(mfrow=c(3,3))
plot(areg1$fitted,rstudent(areg1), main="CAPM", col=3, pch=20, cex=1.5)
plot(areg2$fitted,rstudent(areg2), main="Five-Factor", col=3, pch=20, cex=1.5)
plot(areg3$fitted,rstudent(areg3), main="Six-Factor", col=3, pch=20, cex=1.5)

#Investigate QQ-Plots
qqnorm(rstudent(areg1),main="Q-Q Plot: CAPM",col=4)
abline(a=0,b=1)
qqnorm(rstudent(areg2),main="Q-Q Plot: Five-Factor",col=4)
abline(a=0,b=1)
qqnorm(rstudent(areg3),main="Q-Q Plot: Six-Factor",col=4)
abline(a=0,b=1)

#Investigate Histogram of Studentized Residuals
hist(rstudent(areg1),main="Studentized Residuals: CAPM",col=5)
hist(rstudent(areg2),main="Studentized Residuals: Five-Factor",col=5)
hist(rstudent(areg3),main="Studentized Residuals: Six-Factor",col=5)

#Based on plots of studentized residuals, Q-Q plots, and histograms of 
#studentized residuals, it is not clear which model is the best.  
#The Q-Q plot demonstrates that the left tail of the CAPM plot may 
#be fatter than the left tail for the other two regressions, and 
#therefore the least appropriate.  The Q-Q plots in all regressions 
#demonstrate fat tails in the distribution.  Additionally, there are 
#several days with very large residuals – I will investigate when 
#these occurred, and if they can be removed from the sample for 
#non-statistical reasons.  

aplsr1 <- rstudent(areg1)
aplsr2 <- rstudent(areg2)
aplsr3 <- rstudent(areg3)

applresid <- data.frame(mar,aplsr1,aplsr2,aplsr3)
View(applresid)

applresid1 <- applresid[ which(abs(aplsr1) >= 5),]
applresid2 <- applresid[ which(abs(aplsr2) >= 5),]
applresid3 <- applresid[ which(abs(aplsr3) >= 5),]

View(applresid1)
View(applresid2)
View(applresid3)

#Should remove 4 dates from regression: 
#20120425 - on April 24, Apple announced that it sold 35.1 million iPhone’s during the first three months of 2012, soundly beating analysts’ expectations
#20130124 - investors grew skeptical about the iPhone maker’s growth prospects. 
#20140424 - Apple agreed to settle major lawsuit  
#20150128 - Apple released 10-Q, recording record profits  

mar2 <- mar[ which(mar$DATE!=20120425 & mar$DATE!=20130124 & mar$DATE!=20140424 & mar$DATE!=20150128),]
#View(mar)
#View(mar2)
#OK - 4 observations successfully removed

ARETminusRF2 <- mar2$AAPL - mar2$RF

MKTminusRF2 <- mar2$MKTminusRF

SMB2 <- mar2$SMB
HML2 <- mar2$HML
RMW2 <- mar2$RMW
CMA2 <- mar2$CMA
MOM2 <- mar2$MOM

areg1_2 <- lm(ARETminusRF2~MKTminusRF2)
areg2_2 <- lm(ARETminusRF2~MKTminusRF2 + SMB2 + HML2 + RMW2 + CMA2)
areg3_2 <- lm(ARETminusRF2~MKTminusRF2 + SMB2 + HML2 + RMW2 + CMA2 + MOM2)

#Investigate Apple Regressions
summary(areg1_2)
summary(areg2_2)
summary(areg3_2)

#Investigate Studentized Residuals
par(mfrow=c(3,3))
plot(areg1_2$fitted,rstudent(areg1_2), main="CAPM", col=3, pch=20, cex=1.5)
plot(areg2_2$fitted,rstudent(areg2_2), main="Five-Factor", col=3, pch=20, cex=1.5)
plot(areg3_2$fitted,rstudent(areg3_2), main="Six-Factor", col=3, pch=20, cex=1.5)

#Investigate QQ-Plots
qqnorm(rstudent(areg1_2),main="Q-Q Plot: CAPM",col=4)
abline(a=0,b=1)
qqnorm(rstudent(areg2_2),main="Q-Q Plot: Five-Factor",col=4)
abline(a=0,b=1)
qqnorm(rstudent(areg3_2),main="Q-Q Plot: Six-Factor",col=4)
abline(a=0,b=1)

#Investigate Histogram of Studentized Residuals
hist(rstudent(areg1_2),main="Studentized Residuals: CAPM",col=5)
hist(rstudent(areg2_2),main="Studentized Residuals: Five-Factor",col=5)
hist(rstudent(areg3_2),main="Studentized Residuals: Six-Factor",col=5)

#As before, based on plots of studentized residuals, Q-Q plots, 
#and histograms of studentized residuals, it is not clear which model 
#is the most suitable for Apple’s daily returns. 

#To determine the best model, I will use an F-Test, which 
#assesses if SSR is big relative to SSE.  

#Use ANOVA for Model Selection
anova(areg1_2,areg2_2,areg3_2)

#The F-tests suggests that Model 2, the five-factor model, 
#has the best overall fit. 

#Calculate AIC
print(AIC_appl <- c(areg1_2=extractAIC(areg1_2)[2],
               areg2_2=extractAIC(areg2_2)[2],
               areg3_2=extractAIC(areg3_2)[2]))

#Calculate BIC

n_appl <- nrow(mar2)
print(n_appl)

print(BIC_appl <- c(areg1_2=extractAIC(areg1_2, k=log(n_appl))[2],
               areg2_2=extractAIC(areg2_2, k=log(n_appl))[2],
               areg3_2=extractAIC(areg3_2, k=log(n_appl))[2]))

#Both AIC and BIC demonstrate that Model 2, the five-factor model, 
#is the best model (of the three specified models) for Apple returns.  

#Calculate Model Probabilities
print(eBIC_appl <- exp(-.5*(BIC_appl-min(BIC_appl))))
round(probs_appl <- eBIC_appl/sum(eBIC_appl), 2)

#Moreover, based on BIC, we are 97% sure that the five-factor model is 
#the best of the specified models (see results below).  The results from 
#AIC/BIC agree with our F-testing selection.  As such, I select the 
#five-factor model as the best model for Apple stock.  


###################Analyzing if UK's law to start wearing seat belts (effective 2/1/1983) was effective in increasing driver safety##################

seat <- read.csv("seatbelt.csv")

names(seat)

seat_sort <- seat[order(seat$year, seat$month), ]

#I will use “number of car drivers killed or seriously injured divided 
#by a car traffic index for distance driven by all drivers” (drivers / kms) 
#as my metric for driver safety.  I will do this because, as distance driven 
#by all drivers increases, we would naturally see more deaths/injuries, 
#even if driver safety remained constant.  

drivers_kms <- seat_sort$drivers / seat_sort$kms

par(mfrow=c(2,1))
plot(drivers_kms, xlab="year", ylab="Drivers Killed or Injured / Distance Driven",
     type="l", col=3, lwd=2, xaxt="n")
axis(1, at=(0:16)*12, labels=1969:1985)

plot(log(drivers_kms), xlab="year", ylab="log(Drivers Killed or Injured / Distance Driven)",
     type="l", col=3, lwd=2, xaxt="n")
axis(1, at=(0:16)*12, labels=1969:1985)

#Log(Drivers Killed or Injured / Distance Driven) appears more stationary 
#than the pure Drivers Killed or Injured / Distance Driven.  Namely, in the 
#untransformed scale, there appears to be a noticeable trend downward from 
#1977 to 1982.  This is less noticeable when using the transformed scale.  
#As such, I will model Log(Drivers Killed or Injured / Distance Driven) as the variable
#I will predict.

#Assess correlation of log(drivers)
acf(log(seat_sort$drivers),lag.max=100)

#Use PACF to determine order of AR(p) model

pacf_test <- data.frame(log_drivers_kms_t = log(drivers_kms[1:169]))

par(mfrow=c(1,1))
pacf(pacf_test)

#We see significance in the 14th lag.  We also see slight significance 
#in the 19th lag.  Considering that 19 lags corresponds to more than 
#1.5 years, and will reduce the amount of data to work with, I will 
#not consider any orders above 14.   


#Candidate Model 1: AR(14)
YX_arp14_mode1  <- data.frame(
			month = seat_sort$month[15:169],
			log_drivers_kms_t = log(drivers_kms[15:169]),
			log_drivers_kms_t_14 = log(drivers_kms[1:155]),
			log_drivers_kms_t_13 = log(drivers_kms[2:156]),
		     	log_drivers_kms_t_12 = log(drivers_kms[3:157]),
			log_drivers_kms_t_11 = log(drivers_kms[4:158]),
			log_drivers_kms_t_10 = log(drivers_kms[5:159]),
			log_drivers_kms_t_9 = log(drivers_kms[6:160]),
			log_drivers_kms_t_8 = log(drivers_kms[7:161]),
			log_drivers_kms_t_7 = log(drivers_kms[8:162]),
			log_drivers_kms_t_6 = log(drivers_kms[9:163]),
			log_drivers_kms_t_5 = log(drivers_kms[10:164]),
			log_drivers_kms_t_4 = log(drivers_kms[11:165]),
			log_drivers_kms_t_3 = log(drivers_kms[12:166]),
			log_drivers_kms_t_2 = log(drivers_kms[13:167]),
			log_drivers_kms_t_1 = log(drivers_kms[14:168]))

View(YX_arp14_mode1)

#Model 1: Use 14 lags in AR(p) model (14 chosen based on PCF Plot)
summary(drivers_reg14 <- lm(log_drivers_kms_t ~ log_drivers_kms_t_1 + log_drivers_kms_t_2 + log_drivers_kms_t_3 + log_drivers_kms_t_4 +
log_drivers_kms_t_5 + log_drivers_kms_t_6 + log_drivers_kms_t_7 + log_drivers_kms_t_8 + log_drivers_kms_t_9 + 
log_drivers_kms_t_10 + log_drivers_kms_t_11 + log_drivers_kms_t_12 + log_drivers_kms_t_13 + log_drivers_kms_t_14, data=YX_arp14_mode1))

par(mfrow=c(1,2))
plot(drivers_reg14$resid, xlab="year", ylab="residual", type="l",
     col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:16)*12, labels=1969:1985)
acf(drivers_reg14$resid, lwd=2)

#Autocorrelations plot demonstrates that there are no significant lags.  
#This appears to be an acceptable candidate model.

#Candidate Model 2: Auto-Regressive model with only statistically significant lags (per PACF plot)
summary(drivers_reg_select14 <- lm(log_drivers_kms_t ~ log_drivers_kms_t_1 + log_drivers_kms_t_2 + log_drivers_kms_t_3 + 
log_drivers_kms_t_5 + log_drivers_kms_t_8 + log_drivers_kms_t_9 + 
log_drivers_kms_t_10 + log_drivers_kms_t_11 + log_drivers_kms_t_14, data=YX_arp14_mode1))

par(mfrow=c(1,2))
plot(drivers_reg_select14$resid, xlab="year", ylab="residual", type="l",
     col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:16)*12, labels=1969:1985)
acf(drivers_reg_select14$resid, lwd=2)

#Autocorrelations plot demonstrates that there are still significant AR orders.  
#Using the 1, 2, 3, 5, 8, 9, 10, 11, and 14-month lags fails to capture the 
#entire relationship – there is still information in the past we can use to 
#make a better model.  

#Candidate Model 3: AR(14), with dummies for November and December (cold weather, with icier roads)

YX_arp14_mode1$nov_annual <- YX_arp14_mode1$month == 11
YX_arp14_mode1$dec_annual <- YX_arp14_mode1$month == 12

summary(drivers_reg14_nov_dec <- lm(log_drivers_kms_t ~ log_drivers_kms_t_1 + log_drivers_kms_t_2 + log_drivers_kms_t_3 + log_drivers_kms_t_4 +
log_drivers_kms_t_5 + log_drivers_kms_t_6 + log_drivers_kms_t_7 + log_drivers_kms_t_8 + log_drivers_kms_t_9 + 
log_drivers_kms_t_10 + log_drivers_kms_t_11 + log_drivers_kms_t_12 + log_drivers_kms_t_13 + log_drivers_kms_t_14 + nov_annual + dec_annual, data=YX_arp14_mode1))

par(mfrow=c(1,2))
plot(drivers_reg14_nov_dec$resid, xlab="year", ylab="residual", type="l",
     col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:16)*12, labels=1969:1985)
acf(drivers_reg14_nov_dec$resid, lwd=2)

#Autocorrelations plot demonstrates that there are no significant lags.  
#This appears to be an acceptable candidate model.  Using November and 
#December dummies appears to depress the magnitude of the autocorrelations.  
#As such, I have a slight preference for this model over the pure AR(14) 
#model (Candidate Model 1).

#Candidate Model 4: AR(14), with dummies for November and December, and with price of gas included
#There is a non-trivial correlation between Petrol and KMS (0.38).  My hypothesis is that people 
#drive more miles and more recklessly when petrol prices are low, and fewer miles 
#and more prudently when petrol prices are high.  

YX_arp14_mode1$petrol <- seat_sort$petrol[15:169]
View(YX_arp14_mode1)

summary(drivers_reg14_nov_dec_petrol <- lm(log_drivers_kms_t ~ log_drivers_kms_t_1 + log_drivers_kms_t_2 + log_drivers_kms_t_3 + log_drivers_kms_t_4 +
log_drivers_kms_t_5 + log_drivers_kms_t_6 + log_drivers_kms_t_7 + log_drivers_kms_t_8 + log_drivers_kms_t_9 + 
log_drivers_kms_t_10 + log_drivers_kms_t_11 + log_drivers_kms_t_12 + log_drivers_kms_t_13 + log_drivers_kms_t_14 + nov_annual + dec_annual + petrol, data=YX_arp14_mode1))

par(mfrow=c(1,2))
plot(drivers_reg14_nov_dec_petrol$resid, xlab="year", ylab="residual", type="l",
     col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:16)*12, labels=1969:1985)
acf(drivers_reg14_nov_dec_petrol$resid, lwd=2)

#Autocorrelations plot demonstrates that there are no significant lags.  
#This appears to be an acceptable candidate model.  There does not appear 
#to be a considerable difference in the ACF plot between Candidate Model 3 
#and Candidate Model 4.  

cor(seat_sort$petrol,seat_sort$kms)

#Candidate Model 5: Regress y_t on y_t minus 12, considering we see seasonal effects.
summary(drivers_reg_t12_lag <- lm(log_drivers_kms_t ~ log_drivers_kms_t_12, data=YX_arp14_mode1))

par(mfrow=c(1,2))
plot(drivers_reg_t12_lag$resid, xlab="year", ylab="residual", type="l",
     col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:16)*12, labels=1969:1985)
acf(drivers_reg_t12_lag$resid, lwd=2)

#The ACF plot demonstrates that lags 1, 2, 3, 5, 10, 12, and 18 are significant, 
#demonstrating that there is still information I can leverage from the past to 
#improve the model.  Neither Candidate Model 5 nor 2 is appropriate.

names(drivers_reg14)
View(drivers_reg14$coefficients)

int1 <- summary(drivers_reg14)$coefficients[1,1]
lag1 <- summary(drivers_reg14)$coefficients[2,1]
lag2 <- summary(drivers_reg14)$coefficients[3,1]
lag3 <- summary(drivers_reg14)$coefficients[4,1]
lag4 <- summary(drivers_reg14)$coefficients[5,1]
lag5 <- summary(drivers_reg14)$coefficients[6,1]
lag6 <- summary(drivers_reg14)$coefficients[7,1]
lag7 <- summary(drivers_reg14)$coefficients[8,1]
lag8 <- summary(drivers_reg14)$coefficients[9,1]
lag9 <- summary(drivers_reg14)$coefficients[10,1]
lag10 <- summary(drivers_reg14)$coefficients[11,1]
lag11 <- summary(drivers_reg14)$coefficients[12,1]
lag12 <- summary(drivers_reg14)$coefficients[13,1]
lag13 <- summary(drivers_reg14)$coefficients[14,1]
lag14 <- summary(drivers_reg14)$coefficients[15,1]

print(int1)
print(lag1)
print(lag2)
print(lag3)
print(lag4)
print(lag5)
print(lag6)
print(lag7)
print(lag8)
print(lag9)
print(lag10)
print(lag11)
print(lag12)
print(lag13)
print(lag14)

View(YX_arp14_mode1)

#Calculate Rolling Window for Candidate Model 1
for(k in 1:155)
{

if (k == 1){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k,14]*lag3 + 
YX_arp14_mode1[k,15]*lag2 + YX_arp14_mode1[k,16]*lag1
}

else if (k == 2){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k,14]*lag3 + 
YX_arp14_mode1[k,15]*lag2 + YX_arp14_mode1[k-1,20]*lag1
}

else if (k == 3){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k,14]*lag3 + 
YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2
}

else if (k == 4){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k-1,20]*lag1 + 
YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3
}

else if (k == 5){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2 + 
YX_arp14_mode1[k-3,20]*lag3 + YX_arp14_mode1[k-4,20]*lag4
}

else if (k == 6){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3 + 
YX_arp14_mode1[k-4,20]*lag4 + YX_arp14_mode1[k-5,20]*lag5
}

else if (k == 7){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3 + YX_arp14_mode1[k-4,20]*lag4 + 
YX_arp14_mode1[k-5,20]*lag5 + YX_arp14_mode1[k-6,20]*lag6
}

else if (k == 8){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3 + YX_arp14_mode1[k-4,20]*lag4 + YX_arp14_mode1[k-5,20]*lag5 + 
YX_arp14_mode1[k-6,20]*lag6 + YX_arp14_mode1[k-7,20]*lag7
}

else if (k == 9){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3 + YX_arp14_mode1[k-4,20]*lag4 + YX_arp14_mode1[k-5,20]*lag5 + YX_arp14_mode1[k-6,20]*lag6 + 
YX_arp14_mode1[k-7,20]*lag7 + YX_arp14_mode1[k-8,20]*lag8
}

else if (k == 10){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
 YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3 + YX_arp14_mode1[k-4,20]*lag4 + YX_arp14_mode1[k-5,20]*lag5 + YX_arp14_mode1[k-6,20]*lag6 + YX_arp14_mode1[k-7,20]*lag7 + 
YX_arp14_mode1[k-8,20]*lag8 + YX_arp14_mode1[k-9,20]*lag9
}

else if (k == 11){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k-1,20]*lag1 + 
 YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3 + YX_arp14_mode1[k-4,20]*lag4 + YX_arp14_mode1[k-5,20]*lag5 + YX_arp14_mode1[k-6,20]*lag6 + YX_arp14_mode1[k-7,20]*lag7 + YX_arp14_mode1[k-8,20]*lag8 + 
YX_arp14_mode1[k-9,20]*lag9 + YX_arp14_mode1[k-10,20]*lag10
}

else if (k == 12){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k-1,20]*lag11 + YX_arp14_mode1[k-2,20]*lag2 + 
 YX_arp14_mode1[k-3,20]*lag3 + YX_arp14_mode1[k-4,20]*lag4 + YX_arp14_mode1[k-5,20]*lag5 + YX_arp14_mode1[k-6,20]*lag6 + YX_arp14_mode1[k-7,20]*lag7 + YX_arp14_mode1[k-8,20]*lag8 + YX_arp14_mode1[k-9,20]*lag9 + 
YX_arp14_mode1[k-10,20]*lag10 + YX_arp14_mode1[k-11,20]*lag11
}

else if (k == 13){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3 + 
 YX_arp14_mode1[k-4,20]*lag4 + YX_arp14_mode1[k-5,20]*lag5 + YX_arp14_mode1[k-6,20]*lag6 + YX_arp14_mode1[k-7,20]*lag7 + YX_arp14_mode1[k-8,20]*lag8 + YX_arp14_mode1[k-9,20]*lag9 + YX_arp14_mode1[k-10,20]*lag10 + 
YX_arp14_mode1[k-11,20]*lag11 + YX_arp14_mode1[k-12,20]*lag12
}

else if (k == 14){
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3 + YX_arp14_mode1[k-4,20]*lag4 + 
 YX_arp14_mode1[k-5,20]*lag5 + YX_arp14_mode1[k-6,20]*lag6 + YX_arp14_mode1[k-7,20]*lag7 + YX_arp14_mode1[k-8,20]*lag8 + YX_arp14_mode1[k-9,20]*lag9 + YX_arp14_mode1[k-10,20]*lag10 + YX_arp14_mode1[k-11,20]*lag11 + 
YX_arp14_mode1[k-12,20]*lag12 + YX_arp14_mode1[k-13,20]*lag13
}

else{
YX_arp14_mode1[k,20] <- int1 + YX_arp14_mode1[k-1,20]*lag1 + YX_arp14_mode1[k-2,20]*lag2 + YX_arp14_mode1[k-3,20]*lag3 + YX_arp14_mode1[k-4,20]*lag4 + YX_arp14_mode1[k-5,20]*lag5 + 
 YX_arp14_mode1[k-6,20]*lag6 + YX_arp14_mode1[k-7,20]*lag7 + YX_arp14_mode1[k-8,20]*lag8 + YX_arp14_mode1[k-9,20]*lag9 + YX_arp14_mode1[k-10,20]*lag10 + YX_arp14_mode1[k-11,20]*lag11 + YX_arp14_mode1[k-12,20]*lag12 + 
YX_arp14_mode1[k-13,20]*lag13 + YX_arp14_mode1[k-14,20]*lag14
}

}

#Calculate Rolling Window for Candidate Model 3

View(drivers_reg14_nov_dec$coefficients)

int1 <- summary(drivers_reg14_nov_dec)$coefficients[1,1]
lag1 <- summary(drivers_reg14_nov_dec)$coefficients[2,1]
lag2 <- summary(drivers_reg14_nov_dec)$coefficients[3,1]
lag3 <- summary(drivers_reg14_nov_dec)$coefficients[4,1]
lag4 <- summary(drivers_reg14_nov_dec)$coefficients[5,1]
lag5 <- summary(drivers_reg14_nov_dec)$coefficients[6,1]
lag6 <- summary(drivers_reg14_nov_dec)$coefficients[7,1]
lag7 <- summary(drivers_reg14_nov_dec)$coefficients[8,1]
lag8 <- summary(drivers_reg14_nov_dec)$coefficients[9,1]
lag9 <- summary(drivers_reg14_nov_dec)$coefficients[10,1]
lag10 <- summary(drivers_reg14_nov_dec)$coefficients[11,1]
lag11 <- summary(drivers_reg14_nov_dec)$coefficients[12,1]
lag12 <- summary(drivers_reg14_nov_dec)$coefficients[13,1]
lag13 <- summary(drivers_reg14_nov_dec)$coefficients[14,1]
lag14 <- summary(drivers_reg14_nov_dec)$coefficients[15,1]
nov_coeff <- summary(drivers_reg14_nov_dec)$coefficients[16,1]
dec_coeff <- summary(drivers_reg14_nov_dec)$coefficients[17,1]

print(int1)
print(lag1)
print(lag2)
print(lag3)
print(lag4)
print(lag5)
print(lag6)
print(lag7)
print(lag8)
print(lag9)
print(lag10)
print(lag11)
print(lag12)
print(lag13)
print(lag14)
print(nov_coeff)
print(dec_coeff)


YX_arp14_mode1$nov_dummy <- ifelse(YX_arp14_mode1$nov_annual==TRUE,1,0)
YX_arp14_mode1$dec_dummy <- ifelse(YX_arp14_mode1$dec_annual==TRUE,1,0)

View(YX_arp14_mode1)



for(k in 1:155)
{

if (k == 1){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k,14]*lag3 + 
YX_arp14_mode1[k,15]*lag2 + YX_arp14_mode1[k,16]*lag1 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 2){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k,14]*lag3 + 
YX_arp14_mode1[k,15]*lag2 + YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 3){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k,14]*lag3 + 
YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 4){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k-1,23]*lag1 + 
YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 5){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + 
YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k-4,23]*lag4 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 6){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + 
YX_arp14_mode1[k-4,23]*lag4 + YX_arp14_mode1[k-5,23]*lag5 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 7){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k-4,23]*lag4 + 
YX_arp14_mode1[k-5,23]*lag5 + YX_arp14_mode1[k-6,23]*lag6 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 8){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k-4,23]*lag4 + YX_arp14_mode1[k-5,23]*lag5 + 
YX_arp14_mode1[k-6,23]*lag6 + YX_arp14_mode1[k-7,23]*lag7 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 9){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k-4,23]*lag4 + YX_arp14_mode1[k-5,23]*lag5 + YX_arp14_mode1[k-6,23]*lag6 + 
YX_arp14_mode1[k-7,23]*lag7 + YX_arp14_mode1[k-8,23]*lag8 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 10){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
 YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k-4,23]*lag4 + YX_arp14_mode1[k-5,23]*lag5 + YX_arp14_mode1[k-6,23]*lag6 + YX_arp14_mode1[k-7,23]*lag7 + 
YX_arp14_mode1[k-8,23]*lag8 + YX_arp14_mode1[k-9,23]*lag9 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 11){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k-1,23]*lag1 + 
 YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k-4,23]*lag4 + YX_arp14_mode1[k-5,23]*lag5 + YX_arp14_mode1[k-6,23]*lag6 + YX_arp14_mode1[k-7,23]*lag7 + YX_arp14_mode1[k-8,23]*lag8 + 
YX_arp14_mode1[k-9,23]*lag9 + YX_arp14_mode1[k-10,23]*lag10 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 12){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k-1,23]*lag11 + YX_arp14_mode1[k-2,23]*lag2 + 
 YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k-4,23]*lag4 + YX_arp14_mode1[k-5,23]*lag5 + YX_arp14_mode1[k-6,23]*lag6 + YX_arp14_mode1[k-7,23]*lag7 + YX_arp14_mode1[k-8,23]*lag8 + YX_arp14_mode1[k-9,23]*lag9 + 
YX_arp14_mode1[k-10,23]*lag10 + YX_arp14_mode1[k-11,23]*lag11 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 13){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + 
 YX_arp14_mode1[k-4,23]*lag4 + YX_arp14_mode1[k-5,23]*lag5 + YX_arp14_mode1[k-6,23]*lag6 + YX_arp14_mode1[k-7,23]*lag7 + YX_arp14_mode1[k-8,23]*lag8 + YX_arp14_mode1[k-9,23]*lag9 + YX_arp14_mode1[k-10,23]*lag10 + 
YX_arp14_mode1[k-11,23]*lag11 + YX_arp14_mode1[k-12,23]*lag12 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else if (k == 14){
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k-4,23]*lag4 + 
 YX_arp14_mode1[k-5,23]*lag5 + YX_arp14_mode1[k-6,23]*lag6 + YX_arp14_mode1[k-7,23]*lag7 + YX_arp14_mode1[k-8,23]*lag8 + YX_arp14_mode1[k-9,23]*lag9 + YX_arp14_mode1[k-10,23]*lag10 + YX_arp14_mode1[k-11,23]*lag11 + 
YX_arp14_mode1[k-12,23]*lag12 + YX_arp14_mode1[k-13,23]*lag13 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

else{
YX_arp14_mode1[k,23] <- int1 + YX_arp14_mode1[k-1,23]*lag1 + YX_arp14_mode1[k-2,23]*lag2 + YX_arp14_mode1[k-3,23]*lag3 + YX_arp14_mode1[k-4,23]*lag4 + YX_arp14_mode1[k-5,23]*lag5 + 
 YX_arp14_mode1[k-6,23]*lag6 + YX_arp14_mode1[k-7,23]*lag7 + YX_arp14_mode1[k-8,23]*lag8 + YX_arp14_mode1[k-9,23]*lag9 + YX_arp14_mode1[k-10,23]*lag10 + YX_arp14_mode1[k-11,23]*lag11 + YX_arp14_mode1[k-12,23]*lag12 + 
YX_arp14_mode1[k-13,23]*lag13 + YX_arp14_mode1[k-14,23]*lag14 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff
}

}

View(YX_arp14_mode1)


#Calculate Rolling Window for Candidate Model 4

View(drivers_reg14_nov_dec_petrol$coefficients)

int1 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[1,1]
lag1 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[2,1]
lag2 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[3,1]
lag3 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[4,1]
lag4 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[5,1]
lag5 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[6,1]
lag6 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[7,1]
lag7 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[8,1]
lag8 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[9,1]
lag9 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[10,1]
lag10 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[11,1]
lag11 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[12,1]
lag12 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[13,1]
lag13 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[14,1]
lag14 <- summary(drivers_reg14_nov_dec_petrol)$coefficients[15,1]
nov_coeff <- summary(drivers_reg14_nov_dec_petrol)$coefficients[16,1]
dec_coeff <- summary(drivers_reg14_nov_dec_petrol)$coefficients[17,1]
petrol_coeff <- summary(drivers_reg14_nov_dec_petrol)$coefficients[18,1]

print(int1)
print(lag1)
print(lag2)
print(lag3)
print(lag4)
print(lag5)
print(lag6)
print(lag7)
print(lag8)
print(lag9)
print(lag10)
print(lag11)
print(lag12)
print(lag13)
print(lag14)
print(nov_coeff)
print(dec_coeff)
print(petrol_coeff)


View(YX_arp14_mode1)


for(k in 1:155)
{

if (k == 1){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k,14]*lag3 + 
YX_arp14_mode1[k,15]*lag2 + YX_arp14_mode1[k,16]*lag1 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 2){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k,14]*lag3 + 
YX_arp14_mode1[k,15]*lag2 + YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 3){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k,14]*lag3 + 
YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 4){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k,13]*lag4 + YX_arp14_mode1[k-1,24]*lag1 + 
YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 5){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k,12]*lag5 + YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + 
YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k-4,24]*lag4 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 6){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k,11]*lag6 + YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + 
YX_arp14_mode1[k-4,24]*lag4 + YX_arp14_mode1[k-5,24]*lag5 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 7){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k,10]*lag7 + YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k-4,24]*lag4 + 
YX_arp14_mode1[k-5,24]*lag5 + YX_arp14_mode1[k-6,24]*lag6 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 8){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k,9]*lag8 + YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k-4,24]*lag4 + YX_arp14_mode1[k-5,24]*lag5 + 
YX_arp14_mode1[k-6,24]*lag6 + YX_arp14_mode1[k-7,24]*lag7 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 9){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
YX_arp14_mode1[k,8]*lag9 + YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k-4,24]*lag4 + YX_arp14_mode1[k-5,24]*lag5 + YX_arp14_mode1[k-6,24]*lag6 + 
YX_arp14_mode1[k-7,24]*lag7 + YX_arp14_mode1[k-8,24]*lag8 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 10){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k,7]*lag10 + 
 YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k-4,24]*lag4 + YX_arp14_mode1[k-5,24]*lag5 + YX_arp14_mode1[k-6,24]*lag6 + YX_arp14_mode1[k-7,24]*lag7 + 
YX_arp14_mode1[k-8,24]*lag8 + YX_arp14_mode1[k-9,24]*lag9 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 11){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k,6]*lag11 + YX_arp14_mode1[k-1,24]*lag1 + 
 YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k-4,24]*lag4 + YX_arp14_mode1[k-5,24]*lag5 + YX_arp14_mode1[k-6,24]*lag6 + YX_arp14_mode1[k-7,24]*lag7 + YX_arp14_mode1[k-8,24]*lag8 + 
YX_arp14_mode1[k-9,24]*lag9 + YX_arp14_mode1[k-10,24]*lag10 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 12){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k,5]*lag12 + YX_arp14_mode1[k-1,24]*lag11 + YX_arp14_mode1[k-2,24]*lag2 + 
 YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k-4,24]*lag4 + YX_arp14_mode1[k-5,24]*lag5 + YX_arp14_mode1[k-6,24]*lag6 + YX_arp14_mode1[k-7,24]*lag7 + YX_arp14_mode1[k-8,24]*lag8 + YX_arp14_mode1[k-9,24]*lag9 + 
YX_arp14_mode1[k-10,24]*lag10 + YX_arp14_mode1[k-11,24]*lag11 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 13){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k,4]*lag13 + YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + 
 YX_arp14_mode1[k-4,24]*lag4 + YX_arp14_mode1[k-5,24]*lag5 + YX_arp14_mode1[k-6,24]*lag6 + YX_arp14_mode1[k-7,24]*lag7 + YX_arp14_mode1[k-8,24]*lag8 + YX_arp14_mode1[k-9,24]*lag9 + YX_arp14_mode1[k-10,24]*lag10 + 
YX_arp14_mode1[k-11,24]*lag11 + YX_arp14_mode1[k-12,24]*lag12 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else if (k == 14){
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k,3]*lag14 + YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k-4,24]*lag4 + 
 YX_arp14_mode1[k-5,24]*lag5 + YX_arp14_mode1[k-6,24]*lag6 + YX_arp14_mode1[k-7,24]*lag7 + YX_arp14_mode1[k-8,24]*lag8 + YX_arp14_mode1[k-9,24]*lag9 + YX_arp14_mode1[k-10,24]*lag10 + YX_arp14_mode1[k-11,24]*lag11 + 
YX_arp14_mode1[k-12,24]*lag12 + YX_arp14_mode1[k-13,24]*lag13 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

else{
YX_arp14_mode1[k,24] <- int1 + YX_arp14_mode1[k-1,24]*lag1 + YX_arp14_mode1[k-2,24]*lag2 + YX_arp14_mode1[k-3,24]*lag3 + YX_arp14_mode1[k-4,24]*lag4 + YX_arp14_mode1[k-5,24]*lag5 + 
 YX_arp14_mode1[k-6,24]*lag6 + YX_arp14_mode1[k-7,24]*lag7 + YX_arp14_mode1[k-8,24]*lag8 + YX_arp14_mode1[k-9,24]*lag9 + YX_arp14_mode1[k-10,24]*lag10 + YX_arp14_mode1[k-11,24]*lag11 + YX_arp14_mode1[k-12,24]*lag12 + 
YX_arp14_mode1[k-13,24]*lag13 + YX_arp14_mode1[k-14,24]*lag14 + YX_arp14_mode1[k,21]*nov_coeff + YX_arp14_mode1[k,22]*dec_coeff + YX_arp14_mode1[k,19]*petrol_coeff
}

}

View(YX_arp14_mode1)



error_arp14_model <- YX_arp14_mode1[,20] - YX_arp14_mode1$log_drivers_kms_t
error_arp14_nov_dec_model <- YX_arp14_mode1[,23] - YX_arp14_mode1$log_drivers_kms_t
error_arp14_nov_dec_petrol_model <- YX_arp14_mode1[,24] - YX_arp14_mode1$log_drivers_kms_t

########################## Output Mean Squared Errors ##########################

mean(error_arp14_model^2)
mean(error_arp14_nov_dec_model^2)
mean(error_arp14_nov_dec_petrol_model^2)

#Based on my results, I will select Candidate Model 4 as the best model, 
#considering that there are no statistically significant lags in the ACF plot, 
#and that it has the lowest mean squared error of the models considered.  

########################## Output Mean Squared Errors ##########################

#Now, using candidate model 4, I will predict log(drivers/kms) after the 
#compulsory seat belt was introduced (February 1983 through December 1984)

extrapolate_cm4  <- data.frame(
			month = seat_sort$month[170:192],
			log_drivers_kms_t = log(drivers_kms[170:192]),
			log_drivers_kms_t_14 = log(drivers_kms[156:178]),
			log_drivers_kms_t_13 = log(drivers_kms[157:179]),
		     	log_drivers_kms_t_12 = log(drivers_kms[158:180]),
			log_drivers_kms_t_11 = log(drivers_kms[159:181]),
			log_drivers_kms_t_10 = log(drivers_kms[160:182]),
			log_drivers_kms_t_9 = log(drivers_kms[161:183]),
			log_drivers_kms_t_8 = log(drivers_kms[162:184]),
			log_drivers_kms_t_7 = log(drivers_kms[163:185]),
			log_drivers_kms_t_6 = log(drivers_kms[164:186]),
			log_drivers_kms_t_5 = log(drivers_kms[165:187]),
			log_drivers_kms_t_4 = log(drivers_kms[166:188]),
			log_drivers_kms_t_3 = log(drivers_kms[167:189]),
			log_drivers_kms_t_2 = log(drivers_kms[168:190]),
			log_drivers_kms_t_1 = log(drivers_kms[169:191]),
			petrol_t = seat_sort$petrol[170:192])

extrapolate_cm4$nov_annual <- extrapolate_cm4$month == 11
extrapolate_cm4$dec_annual <- extrapolate_cm4$month == 12

extrapolate_cm4$nov_dummy <- ifelse(extrapolate_cm4$nov_annual == TRUE,1,0)
extrapolate_cm4$dec_dummy <- ifelse(extrapolate_cm4$dec_annual == TRUE,1,0)

View(extrapolate_cm4)

for(k in 1:23)
{

if (k == 1){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
extrapolate_cm4[k,8]*lag9 + extrapolate_cm4[k,9]*lag8 + extrapolate_cm4[k,10]*lag7 + extrapolate_cm4[k,11]*lag6 + extrapolate_cm4[k,12]*lag5 + extrapolate_cm4[k,13]*lag4 + extrapolate_cm4[k,14]*lag3 + 
extrapolate_cm4[k,15]*lag2 + extrapolate_cm4[k,16]*lag1 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 2){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
extrapolate_cm4[k,8]*lag9 + extrapolate_cm4[k,9]*lag8 + extrapolate_cm4[k,10]*lag7 + extrapolate_cm4[k,11]*lag6 + extrapolate_cm4[k,12]*lag5 + extrapolate_cm4[k,13]*lag4 + extrapolate_cm4[k,14]*lag3 + 
extrapolate_cm4[k,15]*lag2 + extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 3){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
extrapolate_cm4[k,8]*lag9 + extrapolate_cm4[k,9]*lag8 + extrapolate_cm4[k,10]*lag7 + extrapolate_cm4[k,11]*lag6 + extrapolate_cm4[k,12]*lag5 + extrapolate_cm4[k,13]*lag4 + extrapolate_cm4[k,14]*lag3 + 
extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 4){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
extrapolate_cm4[k,8]*lag9 + extrapolate_cm4[k,9]*lag8 + extrapolate_cm4[k,10]*lag7 + extrapolate_cm4[k,11]*lag6 + extrapolate_cm4[k,12]*lag5 + extrapolate_cm4[k,13]*lag4 + extrapolate_cm4[k-1,22]*lag1 + 
extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 5){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
extrapolate_cm4[k,8]*lag9 + extrapolate_cm4[k,9]*lag8 + extrapolate_cm4[k,10]*lag7 + extrapolate_cm4[k,11]*lag6 + extrapolate_cm4[k,12]*lag5 + extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + 
extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k-4,22]*lag4 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 6){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
extrapolate_cm4[k,8]*lag9 + extrapolate_cm4[k,9]*lag8 + extrapolate_cm4[k,10]*lag7 + extrapolate_cm4[k,11]*lag6 + extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + 
extrapolate_cm4[k-4,22]*lag4 + extrapolate_cm4[k-5,22]*lag5 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 7){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
extrapolate_cm4[k,8]*lag9 + extrapolate_cm4[k,9]*lag8 + extrapolate_cm4[k,10]*lag7 + extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k-4,22]*lag4 + 
extrapolate_cm4[k-5,22]*lag5 + extrapolate_cm4[k-6,22]*lag6 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 8){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
extrapolate_cm4[k,8]*lag9 + extrapolate_cm4[k,9]*lag8 + extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k-4,22]*lag4 + extrapolate_cm4[k-5,22]*lag5 + 
extrapolate_cm4[k-6,22]*lag6 + extrapolate_cm4[k-7,22]*lag7 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 9){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
extrapolate_cm4[k,8]*lag9 + extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k-4,22]*lag4 + extrapolate_cm4[k-5,22]*lag5 + extrapolate_cm4[k-6,22]*lag6 + 
extrapolate_cm4[k-7,22]*lag7 + extrapolate_cm4[k-8,22]*lag8 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 10){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k,7]*lag10 + 
 extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k-4,22]*lag4 + extrapolate_cm4[k-5,22]*lag5 + extrapolate_cm4[k-6,22]*lag6 + extrapolate_cm4[k-7,22]*lag7 + 
extrapolate_cm4[k-8,22]*lag8 + extrapolate_cm4[k-9,22]*lag9 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 11){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k,6]*lag11 + extrapolate_cm4[k-1,22]*lag1 + 
 extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k-4,22]*lag4 + extrapolate_cm4[k-5,22]*lag5 + extrapolate_cm4[k-6,22]*lag6 + extrapolate_cm4[k-7,22]*lag7 + extrapolate_cm4[k-8,22]*lag8 + 
extrapolate_cm4[k-9,22]*lag9 + extrapolate_cm4[k-10,22]*lag10 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 12){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k,5]*lag12 + extrapolate_cm4[k-1,22]*lag11 + extrapolate_cm4[k-2,22]*lag2 + 
 extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k-4,22]*lag4 + extrapolate_cm4[k-5,22]*lag5 + extrapolate_cm4[k-6,22]*lag6 + extrapolate_cm4[k-7,22]*lag7 + extrapolate_cm4[k-8,22]*lag8 + extrapolate_cm4[k-9,22]*lag9 + 
extrapolate_cm4[k-10,22]*lag10 + extrapolate_cm4[k-11,22]*lag11 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 13){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k,4]*lag13 + extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + 
 extrapolate_cm4[k-4,22]*lag4 + extrapolate_cm4[k-5,22]*lag5 + extrapolate_cm4[k-6,22]*lag6 + extrapolate_cm4[k-7,22]*lag7 + extrapolate_cm4[k-8,22]*lag8 + extrapolate_cm4[k-9,22]*lag9 + extrapolate_cm4[k-10,22]*lag10 + 
extrapolate_cm4[k-11,22]*lag11 + extrapolate_cm4[k-12,22]*lag12 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else if (k == 14){
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k,3]*lag14 + extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k-4,22]*lag4 + 
 extrapolate_cm4[k-5,22]*lag5 + extrapolate_cm4[k-6,22]*lag6 + extrapolate_cm4[k-7,22]*lag7 + extrapolate_cm4[k-8,22]*lag8 + extrapolate_cm4[k-9,22]*lag9 + extrapolate_cm4[k-10,22]*lag10 + extrapolate_cm4[k-11,22]*lag11 + 
extrapolate_cm4[k-12,22]*lag12 + extrapolate_cm4[k-13,22]*lag13 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

else{
extrapolate_cm4[k,22] <- int1 + extrapolate_cm4[k-1,22]*lag1 + extrapolate_cm4[k-2,22]*lag2 + extrapolate_cm4[k-3,22]*lag3 + extrapolate_cm4[k-4,22]*lag4 + extrapolate_cm4[k-5,22]*lag5 + 
 extrapolate_cm4[k-6,22]*lag6 + extrapolate_cm4[k-7,22]*lag7 + extrapolate_cm4[k-8,22]*lag8 + extrapolate_cm4[k-9,22]*lag9 + extrapolate_cm4[k-10,22]*lag10 + extrapolate_cm4[k-11,22]*lag11 + extrapolate_cm4[k-12,22]*lag12 + 
extrapolate_cm4[k-13,22]*lag13 + extrapolate_cm4[k-14,22]*lag14 + extrapolate_cm4[k,20]*nov_coeff + extrapolate_cm4[k,21]*dec_coeff + extrapolate_cm4[k,17]*petrol_coeff
}

}

View(extrapolate_cm4)

t_annual <- 15:169
t_extrapolate <- 170:192

par(mfrow=c(1,1))
plot(log(seat_sort$drivers/seat_sort$kms), xlab="year",
     ylab="log(Drivers Killed or Injured / Distance Driven)", main="Effect of Compulsory Seatbelt Law on Driver Safety", type="l", col=4, lty=2,
     xaxt="n", lwd=2)
axis(1, at=(0:16)*12, labels=1969:1985)
lines(t_annual, drivers_reg14_nov_dec_petrol$fitted, col=2, lwd=2)
lines(t_extrapolate, extrapolate_cm4[,22],col="red",lwd=1)
legend("bottomleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))

par(mfrow=c(1,1))
plot((seat_sort$drivers/seat_sort$kms), xlab="year",
     ylab="Drivers Killed or Injured / Distance Driven", main="Effect of Compulsory Seatbelt Law on Driver Safety", type="l", col=4, lty=2,
     xaxt="n", lwd=2)
axis(1, at=(0:16)*12, labels=1969:1985)
lines(t_annual, exp(drivers_reg14_nov_dec_petrol$fitted), col=2, lwd=2)
lines(t_extrapolate, exp(extrapolate_cm4[,22]),col="red",lwd=1)
legend("bottomleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))

final_analysis <- data.frame(
			month = seat_sort$month[170:192],
			year = seat_sort$month[170:192],
			drivers = seat_sort$drivers[170:192],
			petrol = seat_sort$petrol[170:192],
			kms = seat_sort$kms[170:192],
			extrapolated = extrapolate_cm4$V22[1:23]
)


final_analysis$log_drivers_kms_actual = log(final_analysis$drivers/final_analysis$kms)
final_analysis$log_drivers_kms_exp_less_act = final_analysis$extrapolated - final_analysis$log_drivers_kms_actual

#View(final_analysis)

#The predicted series (Drivers Killed or Injured / Distance Driven) lies above the 
#actual series, demonstrating that the compulsory seatbelt law led to a reduced 
#“drivers killed or injured / distance driven” metric.  While the seatbelt law is 
#the most obvious change beginning in February 1983, I have not gathered enough 
#data to determine that the seatbelt law explicitly caused increased driver safety.  
#Perhaps other laws went in place, or perhaps innovations in car technology 
#resulted in safer cars, and therefore safer roads.

###################Telephone company data; building a model to predict customer################ 
###################churn (i.e., leaving the company in favor of a competitor)##################

chn <- read.csv("churn.csv")
names(chn)
View(chn)

######Data Exploration######

#View Correlation Matrix
rs1 <- transform(chn, STATE=as.numeric(factor(STATE)))
rs2 <- transform(rs1, PHONE=as.numeric(factor(PHONE)))
res <- cor(rs2)
round(res,2)

#Construct corresponding scatter plot matrix
pairs(chn[,1:20],pch=20,col=chn$CHURN)

#The perfect correlation between DMIN and DCHARGE, EMIN and ECHARGE, NMIN and 
#NCHARGE, and IMIN and ICHARGE is conveyed above visually.  The high correlation 
#between VMPLAN and NVMAIL is also made evident.  There is a group of points 
#at (0,0), corresponding to customers that have no voice mail plan, and a group 
#of points corresponding to if a customer has a voice mail plan (VMPLAN), and 
#their corresponding number of voice mail messages (NVMAIL).  

#Given the plots above, I will remove the following variables for statistical and intuitive reasons:
#NVMAIL – this variable has a very high correlation with VMPLAN.  When modeling CHURN, I believe that VMPLAN is a more intuitive variable than NVMAIL.  
#DMIN – this variable has a perfect correlation with DCHARGE.  Customers directly factor their amount billed (not day minutes) when deciding whether to leave their company.  As such, I believe that using DCHARGE to model CHURN is more intuitive.
#EMIN – this variable has a perfect correlation with ECHARGE.  Customers directly factor their amount billed (not evening minutes) when deciding whether to leave their company.  As such, I believe that using ECHARGE to model CHURN is more intuitive.
#NMIN – this variable has a perfect correlation with NCHARGE.  Customers directly factor their amount billed (not night minutes) when deciding whether to leave their company.  As such, I believe that using NCHARGE to model CHURN is more intuitive.
#IMIN – this variable has a perfect correlation with ICHARGE.  Customers directly factor their amount billed (not international minutes) when deciding whether to leave their company.  As such, I believe that using ICHARGE to model CHURN is more intuitive.

#Based on my knowledge of what causes customers to churn, I will remove the following variable:
#PHONE – Phone number (outside of area code) is essentially a randomly generated number.  I do not expect this randomly generated number to have any power in modeling CHURN.

names(rs2)

######Logistic Regression Fitting######
#Include only variables for modeling

churn_model  	<- data.frame(
			STATE = chn$STATE,
			ACLENGTH = chn$ACLENGTH,
			INTPLAN = chn$INTPLAN,
			VMPLAN = chn$VMPLAN,
			DCALL = chn$DCALL,
			DCHARGE = chn$DCHARGE,
			ECALL = chn$ECALL,
			ECHARGE = chn$ECHARGE,
			NCALL = chn$NCALL,
			NCHARGE = chn$NCHARGE,
			ICALL = chn$ICALL,
			ICHARGE = chn$ICHARGE,
			CUSCALL = chn$CUSCALL,
			CHURN = chn$CHURN)

names(churn_model)
View(churn_model)

#Use 2,333 records in training data, and 1,000 in testing data
index <- sample(1:nrow(churn_model),2333)
training_churn <- churn_model[index,]
validation_churn <- churn_model[-index,]

summary(fullreg <- glm(CHURN ~ ., family=binomial, data=training_churn))

#In my current regression model, we have 50 dummy variables corresponding to 
#50 states and DC.  This is excessive!  In my final “main effect” 
#regression model, I will only keep dummies on the states that are 
#significant at a 5% level.  The states are as follows: 
#CA
#MI
#MT
#NJ
#NY
#TX

train_churn_model_simplified <- training_churn
train_churn_model_simplified$STATE_CA <- ifelse(training_churn$STATE == "CA",1,0)
train_churn_model_simplified$STATE_MI <- ifelse(training_churn$STATE == "MI",1,0)
train_churn_model_simplified$STATE_MT <- ifelse(training_churn$STATE == "MT",1,0)
train_churn_model_simplified$STATE_NJ <- ifelse(training_churn$STATE == "NJ",1,0)
train_churn_model_simplified$STATE_NY <- ifelse(training_churn$STATE == "NY",1,0)
train_churn_model_simplified$STATE_TX <- ifelse(training_churn$STATE == "TX",1,0)

names(train_churn_model_simplified)
View(train_churn_model_simplified)

#Remove the STATE variable from data frame
train_churn_model_simplified_nos <- train_churn_model_simplified[,-1]
names(train_churn_model_simplified_nos)
View(train_churn_model_simplified_nos)

#Perform another iteration of GLM

summary(fullreg_simplified <- glm(CHURN ~ ., family=binomial, data=train_churn_model_simplified_nos))

fullreg_w_interactions_simplified <- glm(CHURN ~ . + .^2,, family=binomial, data=train_churn_model_simplified_nos)

#For a first pass, we will use forward-stepwise regression model with main effect for each selected covariate

summary(fwd <- step(fullreg_simplified, scope=formula(fullreg_w_interactions_simplified), direction="forward", k=log(length(index))))

#Simplify regression by removing certain covariates (specifically, those that are not significant at a 5% level)
summary(finalreg <- glm(update(formula(fwd),
                              ~.-ACLENGTH-ECALL-NCALL-ICHARGE-STATE_CA-STATE_NY), family=binomial, data=train_churn_model_simplified_nos )) 

#Calculate Model Probabilities
models_prob <- list(main_w_50_states = fullreg, main_w_select_states = fullreg_simplified, full_w_interactions = fullreg_w_interactions_simplified,
forward_step = fwd, final = finalreg)

bic_churn <- sapply(models_prob, extractAIC, k=log(length(index)))[2,]
ebic_churn <- exp(-.5*(bic_churn-min(bic_churn)))
round(ebic_churn/sum(ebic_churn),4)

#Based on BIC model probabilities above, I am essentially 100% confident that the final model (Model 5) 
#is the best of the five models considered.

#Now, calculate MSE's on left out data

val_churn_model_simplified <- validation_churn
val_churn_model_simplified$STATE_CA <- ifelse(validation_churn$STATE == "CA",1,0)
val_churn_model_simplified$STATE_MI <- ifelse(validation_churn$STATE == "MI",1,0)
val_churn_model_simplified$STATE_MT <- ifelse(validation_churn$STATE == "MT",1,0)
val_churn_model_simplified$STATE_NJ <- ifelse(validation_churn$STATE == "NJ",1,0)
val_churn_model_simplified$STATE_NY <- ifelse(validation_churn$STATE == "NY",1,0)
val_churn_model_simplified$STATE_TX <- ifelse(validation_churn$STATE == "TX",1,0)

names(val_churn_model_simplified)
View(val_churn_model_simplified)

#Remove the STATE variable from data frame
val_churn_model_simplified_nos <- val_churn_model_simplified[,-1]

View(val_churn_model_simplified_nos)

#We have five models; we will calculate the mean squared error for each model's predictions over the left-out validation
#dataset.

error_main_w_50_states <- predict(fullreg, newdata=validation_churn, type="response")
error_main_w_select_states <- predict(fullreg_simplified, newdata=val_churn_model_simplified_nos, type="response")
error_full_w_interactions <- predict(fullreg_w_interactions_simplified, newdata=val_churn_model_simplified_nos, type="response")
error_forward_step <- predict(fwd, newdata=val_churn_model_simplified_nos, type="response")
error_final <- predict(finalreg, newdata=val_churn_model_simplified_nos, type="response")

mean(abs(error_main_w_50_states))
mean(abs(error_main_w_select_states))
mean(abs(error_full_w_interactions))
mean(abs(error_forward_step))
mean(abs(error_final))

#In aggregate, all models have a misclassification rate of 13% - 14%, implying that our model 
#classifies whether customers “churn” correctly 86% - 87% of the time.
#For a more detailed analysis, I would perform a simulation over randomly generated “training” 
#and “validation” datasets, and then calculate MSE for each simulation.  
#This would give a more appropriate distribution as to which model fits each validation dataset most appropriately.


#Next, I'd like to provide intuition as to how to interpret the multivariate logistic regression.
#To do this, I would prepare sensitivity analyses to provide more intuition on how to interpret the 
#model.  For example, I would hold all variables but one constant, perturb the variable in question, 
#and report how the probability of churning increases (or decreases) as we perturb each input.  
#Understanding a multivariate logistic model is not trivial, but isolating the model down to one 
#dimension at a time would help explain the mechanics of the model.  

#I have prepared a sensitivity analysis on the “ICHARGE” (Total International Charge) variable.  
#To analyze how CHURN varies with ICHARGE, I analyze all customers with an international plan 
#(INTPLAN = 1) that do not live in MI, MT, NJ, or TX, and hold all the independent variables, 
#except ICHARGE, at their respective mean value.  The relationship between CHURN and 
#ICHARGE is summarized with the chart below:

#Plot interpretation: Assume analyzing INTPLAN = 1
mean_INTPLAN <- 1
mean_VMPLAN <- mean(chn$VMPLAN)
mean_DCALL <- mean(chn$DCALL)
mean_DCHARGE <- mean(chn$DCHARGE)
mean_ECHARGE <- mean(chn$ECHARGE)
mean_NCHARGE <- mean(chn$NCHARGE)
mean_ICALL <- mean(chn$ICALL)
mean_CUSCALL <- mean(chn$CUSCALL)
mean_DCHARGE_CUSCALL <- mean(chn$DCHARGE*chn$CUSCALL)
mean_DCHARGE_ECHARGE <- mean(chn$DCHARGE*chn$ECHARGE)
mean_VMPLAN_DCHARGE <- mean(chn$VMPLAN*chn$DCHARGE)
mean_INTPLAN_ICHARGE <- mean(chn$ICHARGE*chn*ICHARGE)
mean_INTPLAN_ICALL <- mean(chn$INTPLAN*chn$ICALL)
mean_DCHARGE_NCHARGE <- mean(chn$DCHARGE*chn$NCHARGE)
mean_INTPLAN_DCHARGE <- mean(chn$INTPLAN*chn$DCHARGE)
mean_DCHARGE_ICALL <- mean(chn$DCHARGE*chn$ICALL)
mean_ECHARGE_CUSCALL <- mean(chn$ECHARGE*chn$CUSCALL)
mean_INTPLAN_CUSCALL <- mean(cng$INTPLAN*chn$CUSCALL)
mean_DCALL_ECHARGE <- mean(chn$DCALL*chn$ECHARGE)

summary(finalreg)

const0 = coef(finalreg)["(Intercept)"] + mean_VMPLAN*coef(finalreg)["VMPLAN"] + mean_DCALL*coef(finalreg)["DCALL"] + mean_DCHARGE*coef(finalreg)["DCHARGE"] + mean_ECHARGE*coef(finalreg)["ECHARGE"] + mean_NCHARGE*coef(finalreg)["NCHARGE"] + mean_ICALL*coef(finalreg)["ICALL"] + mean_CUSCALL*coef(finalreg)["CUSCALL"] + mean_DCHARGE_CUSCALL*coef(finalreg)["DCHARGE:CUSCALL"] + mean_DCHARGE_ECHARGE*coef(finalreg)["DCHARGE:ECHARGE"] + mean_VMPLAN_DCHARGE*coef(finalreg)["VMPLAN:DCHARGE"] + mean_DCHARGE_NCHARGE*coef(finalreg)["DCHARGE:NCHARGE"] + mean_INTPLAN_CUSCALL*coef(finalreg)["INTPLAN:CUSCALL"] + mean_ECHARGE_CUSCALL*coef(finalreg)["ECHARGE:CUSCALL"] + mean_INTPLAN_ICALL*coef(finalreg)["INTPLAN:ICALL"]
const0

s <- seq(0,10,length=100)
interpretation <- exp(s*coef(finalreg)["INTPLAN:ICHARGE"] + const0)/(1+exp(s*coef(finalreg)["INTPLAN:ICHARGE"] + const0))
interpretation
plot(s, interpretation, typ="l", col=4, lwd=2, ylim=c(0,1), xlab="ICHARGE", main = "Impact of ICHARGE on Churning", ylab="P(CHURN)")

#The chart demonstrates that, for the “average” customer, we see low churning at ICHARGE < 2, an increased rate of churning from ICHARGE > 2 to ICHARGE < 3, 
#and high probability of churning at ICHARGE > 4.  Through this plot, we have statistical insights on how ICHARGE impacts P(CHURN).  

#The above is only one sensitivity analysis, where I conveyed how P(CHURN) varies with ICHARGE.  
#Via a comprehensive sensitivity analysis on all inputs to the model, we can understand which variables are the most important to the model, 
#and based on those results, can modify the business strategy accordingly to prevent customers from churning to other companies.    



