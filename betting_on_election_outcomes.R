#Analysis of Trump twitter feed.

#Let's load in Trump Twitter Feed data
#Not only Donald Trump posts to the Trump twitter feed.  Members of his team post as well.
#Trump posts with an Android; his campaign uses an iPhone

trump <- read.csv("trump.csv")
View(trump)
names(trump)

favoriteCount <- trump$favoriteCount
retweetCount <- trump$retweetCount
source <- trump$source
created <- trump$created
x <- as.POSIXct(created,  format="%m/%d/%Y  %H:%M")
linear1 <- as.numeric(x)
linear <- round((linear1 - min(linear1)) / (3600*24*7),digits = 0)

#Note: Linear is defined as number of weeks from 9/26/2016

summary(test1<-lm(favoriteCount ~ source*linear))

plot(linear,favoriteCount,xlab="Linear Time",ylab="Favorite Count")
#We can see that there are increasing "likes" of the Tweets over time

summary(test2<-lm(trump$retweetCount ~ trump$source))
#This demonstrates that there are fewer retweets when an iPhone is used to post the tweets, as expected.
#We expect tweets from Trump himself (Android) to resonate more with his base

plot(trump$created,trump$favoriteCount,xlab="Created",ylab="Favorite Count")
plot(trump$created,trump$retweetCount,xlab="Created",ylab="Retweet Count")

summary(test1<-lm(favoriteCount ~ source*linear))

#Let's review the data leading in to the election, bifurcating by Android and iPhone
par(mfrow=c(1,2))
boxplot(favoriteCount [source=="Android"] ~ linear[source=="Android"], col=5, main="Android Favorite Tweets",
        xlab="Time from 9/26/2016 (weeks)", ylab="Number of Favorite Counts")
boxplot(favoriteCount [source=="iPhone"] ~ linear[source=="iPhone"], col=5, main="iPhone Favorite Tweets",
        xlab="Time from 9/26/2016 (weeks)", ylab="Number of Favorite Counts")

par(mfrow=c(1,2))
boxplot(retweetCount [source=="Android"] ~ linear[source=="Android"], col=5, main="Android Retweet Counts",
        xlab="Time from 9/26/2016 (weeks)", ylab="Number of Retweet Counts")
boxplot(retweetCount [source=="iPhone"] ~ linear[source=="iPhone"], col=5, main="iPhone Retweet Counts",
        xlab="Time from 9/26/2016 (weeks)", ylab="Number of Retweet Counts")

android_favm <- tapply(favoriteCount [source=="Android"], linear[source=="Android"], mean) 
iphone_favm <- tapply(favoriteCount [source=="iPhone"], linear[source=="iPhone"], mean)

x_dummya <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
x_dummyi <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)


par(mfrow=c(1,1))
plot(x_dummya, android_favm, lwd = 2, col=4, xlab="Time from 9/26/2016 (weeks)",
     ylab="mean(Favorite Count)",main="Comparison between Android and iPhone")
lines(x_dummyi, iphone_favm, col=6)

android_rtwm <- tapply(retweetCount [source=="Android"], linear[source=="Android"], mean) 
iphone_rtwm <- tapply(retweetCount [source=="iPhone"], linear[source=="iPhone"], mean)

par(mfrow=c(1,1))
plot(x_dummya, android_rtwm, lwd = 2, col=4, xlab="Time from 9/26/2016 (weeks)",
     ylab="mean(Retweet Count)",main="Comparison between Android and iPhone")
lines(x_dummyi, iphone_rtwm, col=6)

summary( fc <- lm(favoriteCount ~ source) )
summary( rt <- lm(retweetCount ~ source) )

#For both Retweets and Favorites, it appears that the Android tweets generate larger volume 
#than those of the iPhone, which is congruent with my hypothesis.  
#This is supported by the negative coefficient on the dummy variable for iPhone.  

#Part ii

#Now, let's examine if there are different grammatical patterns between the iPhone and Android posts

hashtag <- ifelse(trump$hashtag == "Yes",1,0)
View(hashtag)

exclamation <- ifelse(trump$exclamation == "Yes",1,0)
View(exclamation)

android_hashm <- tapply(hashtag [source=="Android"], linear[source=="Android"], mean) 
iphone_hashm <- tapply(hashtag [source=="iPhone"], linear[source=="iPhone"], mean)


par(mfrow=c(1,1))
plot(x_dummya, android_hashm, ,lwd=2, col=4, xlab="Time from 9/26/2016 (weeks)",
     ylab="mean(Hashtag Count)",main="Comparison between Android and iPhone",ylim=c(0,1))
lines(x_dummyi, iphone_hashm, col=6)
#Note: Android is represented by blue circles, while iPhone is represented by pink line.

android_exclm <- tapply(exclamation [source=="Android"], linear[source=="Android"], mean) 
iphone_exclm <- tapply(exclamation [source=="iPhone"], linear[source=="iPhone"], mean)

par(mfrow=c(1,1))
plot(x_dummya, android_exclm , lwd = 2, col=4, xlab="Time from 9/26/2016 (weeks)",
     ylab="mean(Exclamation Count)",main="Comparison between Android and iPhone",ylim=c(0,1))
lines(x_dummyi, iphone_exclm, col=6)
#Note: Android is represented by blue circles, while iPhone is represented by pink line.

summary( ht <- lm(hashtag ~ source) )
summary( ex <- lm(exclamation ~ source) )
#The iPhone is associated with more uses of the hashtag.
#The iPhone is associated with fewer uses of the exclamation mark.  

#Part iii
#Now, let's investigate if different patterns influence readers' response to Trump's tweets.

retweetCount <- trump$retweetCount
favoriteCount <- trump$favoriteCount
picture <- trump$picture

piclink_retweetcount <- tapply(retweetCount [picture=="Picture/link"], linear[picture=="Picture/link"], mean) 
npiclink_retweetcount <- tapply(retweetCount [picture=="No picture/link"], linear[picture=="No picture/link"], mean) 

par(mfrow=c(1,1))
plot(x_dummya, piclink_retweetcount, ,lwd=2, col=4, xlab="Time from 9/26/2016 (weeks)",
     ylab="mean(Retweet Count)",main="Comparison between Picture/link and No Picture/link",ylim=c(0,70000))
lines(x_dummya, npiclink_retweetcount, col=6)

piclink_favoritecount <- tapply(favoriteCount [picture=="Picture/link"], linear[picture=="Picture/link"], mean) 
npiclink_favoritecount <- tapply(favoriteCount [picture=="No picture/link"], linear[picture=="No picture/link"], mean) 

par(mfrow=c(1,1))
plot(x_dummya, piclink_favoritecount , lwd = 2, col=4, xlab="Time from 9/26/2016 (weeks)",
     ylab="mean(Favorite Count)",main="Comparison between Picture/link and No Picture/link",ylim=c(0,250000))
lines(x_dummya, npiclink_favoritecount, col=6)

hour <- trump$hour

summary( rtc <- lm(retweetCount ~ picture + hour) )
summary( fvc <- lm(favoriteCount ~ picture + hour) )

#When evaluating retweets, there are fewer retweets when a Picture/link is included.  
#The hour of the tweet does not have a statistically significant relationship to the retweet count.