data<-c(15,20,21,20,36,25,15)
data()
median(data)
sd(data)
var(data)

library(tidyverse)
library(modelr)
options(ns.action = na.warn)
sim1
data()





x<-sim1$x
y<-sim1$y

Sx<-sum(x)
Sy<-sum(y)
Sxy<-sum(x*y)
Sxx<-sum(x*x)
Syy<-sum(y*y)
Sx2<-Sx*Sx
Sy2<-Sy*Sy
n<-length(x)
n


r=(n*Sxy-Sx*Sy)/sqrt((n*Sxx-Sx2)*(n*Syy-Sy2))

cat("r=",r)

plot(sim1)
data()

b<- (n*Sxy-Sx*Sy)/((n*Sxx)-Sx2)
a<-(Sy/n)-((b*Sx)/n)
abline(a,b)



#--------
lm(y~x)
x1<-c(20,16,24,22,18)
y1<-c(82,70,90,85,73)
lm(sim1$x1~sim1$y1)


#btvn:
  #xây dựng hàm tính R
  #tìm dataset và chọn 2 lt
    #tính R
    #xd: y= a*bx
    #vẽ biểu đồ tản xạ
    #vẽ đường thẳng y=a+bx


