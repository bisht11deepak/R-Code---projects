### Linear regression
library(readr)
calorieconsumed<-read.csv("E:/torrent downloaded movies/Assingments/Simple Linear Regression/calories_consumed.csv")
View(calorieconsumed)

install.packages("lattice")
library(lattice)
 
## Exploratory Anlaysis
attach(calorieconsumed)
plot(calorieconsumed$Weight.gained..grams.,calorieconsumed$Calories.Consumed)
cor(Weight.gained..grams.,Calories.Consumed)
qqnorm(calorieconsumed$Weight.gained..grams.,main = "calroie")
qqnorm(calorieconsumed$Calories.Consumed)
qqline(calorieconsumed$Calories.Consumed)
boxplot(calorieconsumed$Weight.gained..grams.,horizontal= T, col = "red")
library(moments)
skewness(calorieconsumed$Weight.gained..grams.)
kurtosis(calorieconsumed$Weight.gained..grams.)

### Simple linear regression
calconsumemodel<-lm(Weight.gained..grams.~Calories.Consumed, data = calorieconsumed)
summary(calconsumemodel)
calconsumemodel$fitted.values
calconsumemodel$residuals

### For better visualizations
install.packages("ggplot2")
library(ggplot2)
ggplot(data=calorieconsumed,aes(x=Calories.Consumed,y=Weight.gained..grams.))+geom_point(color="red")+geom_line(color="green",data=calorieconsumed,aes(x=Calories.Consumed,y=calconsumemodel$fitted.values))
cor(calconsumemodel$fitted.values,calorieconsumed$Calories.Consumed)
 
##### Tranformation for improving the model

calconsumemode2<-lm(sqrt(Weight.gained..grams.)~Calories.Consumed)
summary(calconsumemode2)
predictsqr<-calconsumemode2$fitted.values
predictsqr
predicterror<-calconsumemode2$residuals
predictactual<-(predictsqr)^2
predictactual
predictactualerror<-(predicterror)^2
predictactualerror
ggplot(data=calorieconsumed, aes(x=Calories.Consumed,y=Weight.gained..grams.))+geom_point(color= "blue")+geom_line(color="red",data=calorieconsumed,aes(x=Calories.Consumed,y=predictactual))