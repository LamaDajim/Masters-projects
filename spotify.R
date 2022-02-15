install.packages("dlookr")
install.packages("caTools")
install.packages("tidyverse")
library(caTools)
library(tidyverse)

# Data available here:  
# https://www.kaggle.com/zaheenhamidani/ultimate-spotify-tracks-db
spotify=read.csv('C:/Users/lamad/Downloads/spotify.csv')
#explore
str(spotify)
summary(spotify)
#returns values location if they there are null values
anyNA(spotify)

#checking and removing rows with duplicate track-id 
any(duplicated(spotify$track_id))
spotify=spotify[!duplicated(spotify$track_id),]
any(duplicated(spotify$track_id))
dim(spotify)
#Checking for inconsistencies and dropping/changing the rows
unique(spotify$time_signature)
subset(spotify,spotify$time_signature=="0/4")
spotify<-spotify[!(spotify$time_signature=="0/4"),]
unique(spotify$time_signature)

unique(spotify$ï..genre )
spotify["ï..genre"][spotify["ï..genre"] == "Childrenâ???Ts Music"] <- "Children's Music"
unique(spotify$ï..genre )

#descriptive statistics
dim(spotify)

quantile(spotify$popularity)
mean(spotify$popularity)
var(spotify$popularity)
sd(spotify$popularity)

quantile(spotify$loudness)
mean(spotify$loudness)
sd(spotify$loudness)
var(spotify$loudness)

cov(spotify$loudness,spotify$popularity)

cor(spotify[, c('popularity','acousticness','danceability','duration_ms'
               , 'energy','instrumentalness','liveness','loudness'
                ,'speechiness','tempo','valence')])


#VISUALISATION
spotifySample=spotify[sample(nrow(spotify), 1000), ]
pairs(~popularity+acousticness+danceability+duration_ms
      +energy+instrumentalness+liveness+loudness
      +speechiness+tempo+valence,cex = 0.5 , data=spotifySample)

par("mfcol"=c(3,4))
hist(spotify$popularity)
hist(spotify$acousticness)
hist(spotify$danceability)
hist(spotify$duration_ms)
hist(spotify$energy)
hist(spotify$instrumentalness)
hist(spotify$liveness)
hist(spotify$loudness)
hist(spotify$speechiness)
hist(spotify$tempo)
hist(spotify$valence)
par("mfcol"=c(1,1))

#SPLIT

set.seed(10)
spl=sample.split(spotify$energy,SplitRatio = 0.75)
spotifyTrain = subset(spotify, spl==TRUE)
spotifyTest = subset(spotify, spl==FALSE)

#Linear Regression
model1 <- lm(energy~acousticness+loudness, data = spotifyTrain )
summary(model1)
print(model1)

model2 <- lm(energy~acousticness+danceability
             +popularity+instrumentalness+liveness+loudness
             +speechiness+tempo+valence, data = spotifyTrain )
summary(model2)
print(model2)

model2$residuals
SSE = sum(model2$residuals^2)
SSE


# Make test set predictions
predictTest = predict(model2, newdata=spotifyTest)
predictTest

# Compute R-squared
SSE = sum((spotifyTest$energy - predictTest)^2)
SST = sum((spotifyTest$energy - mean(spotifyTrain$energy))^2)
1 - SSE/SST
