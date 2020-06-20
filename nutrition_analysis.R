#Analysis on nutrition of infants and preschool children

nutr<-read.csv("nutrition.csv")

names(nutr)

reg <- lm(nutr$woh ~ nutr$age)

plot(nutr$age,nutr$woh,pch=20,xlab="Age",ylab="WOH")
abline(lm(nutr$woh ~ nutr$age),col=2)

plot(nutr$age,resid(reg),pch=20,xlab="Age",ylab="Residuals")
#The plot appears to follow a log relationship (that is, WOH should 
#be regressed on log(Age)).

summary(reg)
#In the simple regression of WOH on age, 
#we see a very large F-statistic (318.7), demonstrating that we should 
#reject the null that there is no relationship between WOH and age.

reg2 <- lm(nutr$woh ~ log(nutr$age))
summary(reg2)

plot(log(nutr$age),nutr$woh,pch=20,xlab="log(age)",ylab="WOH")
abline(lm(nutr$woh ~ log(nutr$age)),col=2)

#The fit appears to be much better.  
#This is exemplified by the larger F-statistic: 1046

#Based on the superior F-test, and more randomized plot of residuals against 
#log(age), I would pursue analyzing the relationship between log(age) and WOH.  
#Since the F-statistic is larger, the relationship between WOH and log(age) 
#is more statistically significant. 

plot(log(nutr$age),resid(reg2),pch=20,xlab="log(Age)",ylab="Residuals")

n <- nrow(nutr)

print(BIC <- c(reg=extractAIC(reg, k=log(n))[2],
               reg2=extractAIC(reg2, k=log(n))[2]))

#I prefer REG2 (WOH on log(age)), which has a smaller BIC

#There is a noticeable kink in the relationship between age and WOH around age = 7.  
#To examine this kink/change in the relationship, let's examine if there is a relationship 
#for childrent above/below 7.

woh <- nutr$woh
age <- nutr$age
gt7 <- ifelse(age > 7,1,0)

#View(gt7)
#Quality test - dummy variable created successfully

summary(lm(woh ~ age + gt7 + age*gt7))
#The F-statistic is 597.6, which demonstrates that we can reject 
#the null hypothesis that all betas are 0. 

#The F-statistic for our regression of WOH on log(age), above, is larger 
#than this F-statistic (1046 vs. 597.6, respectively).  
#However, the BIC for reg3, which includes gt7, and the interaction 
#between age and gt7, is smaller than the BIC for reg2 (see below).

reg3 <- lm(woh ~ age + gt7 + age*gt7)

print(BIC <- c(reg=extractAIC(reg2, k=log(n))[2],
               reg2=extractAIC(reg3, k=log(n))[2]))

plot(reg3$fitted,resid(reg3),pch=20,xlab="Fitted Values",ylab="Residuals")

plot(reg2$fitted,rstudent(reg2),pch=20,xlab="Fitted Values",ylab="Residuals")
plot(reg3$fitted,rstudent(reg3),pch=20,xlab="Fitted Values",ylab="Residuals")
#Each residual appears to have a constant variance
#Each residual has the same mean (0)

par(mfrow=c(1,2))
qqnorm(rstudent(reg2),col=4,main="lm(woh ~ log(age)")
abline(a=0,b=1)

qqnorm(rstudent(reg3),col=4,main="lm(woh ~ age + gt7 + age*gt7)")
abline(a=0,b=1)

#The residuals in "Reg3" follow a normal distribution more 
#closely than the residuals in “Reg2,” per Q-Q plots (recall: “Reg3” is acquired via the following linear regression – lm(woh ~ age + gt7 + age*gt7))

#Now, let's inspect prediction intervals for 

log_age <- log(nutr$age)

par(mfrow=c(1,2))
plot(log(nutr$age),nutr$woh,xlab="log(Age)",ylab="WOH",main="lm(woh~log(age))")
r.lm <- lm(nutr$woh ~ log(nutr$age))
PI_ex <- predict(r.lm, interval="prediction")
woh_results <- cbind(woh,log_age,PI_ex)
#woh_results_s <- woh_results[order(woh_results$New),]
lines(x=log_age,y=PI_ex[,2],col=2)
lines(x=log_age,y=PI_ex[,3],col=2)

plot(nutr$age,nutr$woh,xlab="Age",ylab="WOH",main="lm(woh ~ age + gt7 + age*gt7)")
r1.lm <- lm(woh ~ age + gt7 + age*gt7)
PI_ex1 <- predict(r1.lm, interval="prediction")
woh_results <- cbind(woh,log_age,PI_ex1)
lines(x=age,y=PI_ex1[,2],col=2)
lines(x=age,y=PI_ex1[,3],col=2)

#Overall, I prefer the regression model specified by “lm(woh ~ age + gt7 + age*gt7).”  
#I prefer this for two reasons:
#1.	Of the three models considered, this model has the smallest BIC
#2.	There appears to be a change in the in linear the relationship between WOH and age before age 7 and after age 7.  
#The regression model “woh ~ age + gt7 + age*gt7” therefore naturally accommodates for the different 
#linear relationship before age 7 and after age 7. 




