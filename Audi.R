install.packages("caTools")
library(caTools)

# Data is found here:
# https://www.kaggle.com/adityadesai13/used-car-dataset-ford-and-mercedes

#importing dataset
audi=read.csv('C:/Users/lamad/Downloads/sts lab hw/audi.csv')
str(audi)
summary(audi)
#Correlation between all numeric variables
cor(audi[, c('price', 'year', 'mileage','tax','mpg','engineSize')])

#pairwise Scatter plot
pairs(~price+year+mileage+mpg+tax+engineSize , data=audi)

#Splitting dataset into testing and training datasets 
set.seed(10)
spl=sample.split(audi$year,SplitRatio = 0.75)
Train = subset(audi, spl==TRUE)
Test = subset(audi, spl==FALSE)
str(Train)
str(Test)

#Building a linear regression model
RTrain <- lm(price~year+mileage+mpg+tax+engineSize, data = Train)
print(RTrain)
summary(RTrain)
RTrain$residuals
SSE = sum(RTrain$residuals^2)
SSE

#Applying the model to the test data set
predictTest = predict(RTrain, newdata=Test)
predictTest

#R-Squared
SSE = sum((Test$price - predictTest)^2)
SST = sum((Test$price - mean(Train$price))^2)
1-SSE/SST

