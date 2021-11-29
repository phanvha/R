library(tidyverse)
library(modelr)
mtcars
data()
x <- mtcars$am
x
y <- mtcars$gear
y
sx<-sum(x)
sx
sy<-sum(y)
sxy<-sum(x*y)
sxx<-sum(x*x)
syy<-sum(y*y)
sx2<-sx*sx
sy2<-sy*Sy
n<-length(x)
n
plot(mtcars)

