library(readr)
Q9_a<-read_csv("E:/torrent downloaded movies/Assingments/Basic_Statistic_Level _1/Q9_a.csv")
attach(Q9_a)
View(Q9_a)
install.packages("moments")
library("moments")
attach(Q9_a)
skewness(dist)
skewness(speed)
kurtosis(dist)
kurtosis(speed)
Q9_b<-read_csv("E:/torrent downloaded movies/Assingments/Basic_Statistic_Level _1/Q9_b.csv")
View(Q9_b)
attach(Q9_b)
skewness(SP)
skewness(WT)
kurtosis(SP)
a<-kurtosis(WT)
hist(WT)
kurtosis(WT)
xbar<-200      ## calculating cofindence interval
n<-2000
sigma<-30
error1<-qnorm(0.97)*sigma/sqrt(n)
left<-xbar-error1
right<-xbar+error1
left
right
X2<-c(34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56)
mean(X2)
median(X2)
var(X2)
sd(X2)


## calculating the probabilities for different conditions
library(readr)
cardata<-read_csv("E:/torrent downloaded movies/Assingments/Basic_Statistic_Level _1/Cars.csv")
View(cardata)
attach(cardata)
MPG<-cardata$MPG
MPG
hist(MPG)
summary(MPG)
d<-mean(MPG)
q<-sd(MPG)
s<-38
Cardatadist<-pnorm(s,d,q)
Cardatadist
Probgreaterthan38<-1-convertzscore
Probgreaterthan38


d<-mean(MPG)
q<-sd(MPG)
s<-40
Cardatadist<-pnorm(s,d,q)
Cardatadist
Problessthan40<-Cardatadist
Problessthan40

d<-mean(MPG)
q<-sd(MPG)
s<-20
Cardatadist<-pnorm(s,d,q)
Cardatadist
Probgreaterthan20<-Cardatadist
X1<-Probgreaterthan20



d<-mean(MPG)
q<-sd(MPG)
s<-50
Cardatadist<-pnorm(s,d,q)
Cardatadist
Problessthan50<-Cardatadist
X2<-Problessthan50

Probbetween20and50<-(X2-X1)*100  ### calculating the probability with different range of data
Probbetween20and50
qqnorm(cardata$MPG)
qqline(cardata$MPG)
summary(cardata$MPG)
qqnorm(cardata$MPG)
qqline(cardata$MPG)

# ## calculating cofindence interval interval for of data set at different Z values
View(cardata)
attach(cardata)
d<-mean(MPG)   ### Normal disturibution or Z distribution
q<-sd(MPG)
n1<-81
error2<-qnorm(0.75)*q/sqrt(n1)
left22<-d-error2
left22
right22<-d+error2
right22

View(cardata)
attach(cardata)
d<-mean(MPG)   ### T distribution
q<-sd(MPG)
n2<-25
error2<-qt(0.995,df=n2-1)*q/sqrt(n2)
left22<-d-error2
left22
right22<-d+error2
right22

library(readr)
wc_at<-read_csv("E:/torrent downloaded movies/Assingments/Basic_Statistic_Level _1/wc-at.csv")
View(wc_at)
attach(wc_at)
qqnorm(wc_at$Waist, label=Wcwaist)
qqline(wc_at$Waist)
boxplot(wc_at$Waist)
qqnorm(wc_at$AT)
qqline(wc_at$AT)
boxplot(wc_at$AT)
ggplot()

muu=270
xbarr=260
n2=17
s=90
terror<-s/sqrt(n2)
tscore<-(xbarr-muu)/terror
tscore
