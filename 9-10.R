data = read.csv(file="D:\\PhanVanha\\Students.csv",header = TRUE)
data
dim(data)
edit(data)
names(data)

t<-data
t1 <- unique(t)
dim(t1)
t2<-na.omit(t1)
dim(t2)
attach(t2)

#tạo các mô hình đơn biến

#fnbmd~wt(vẽ biểu đồ tán và plots và đường hồi quy abline)
#c1<-t2$wt
#c2<-t2$fnbmd
#plot(c1,c2)
#abline(c1,c2)

#chia cua so plot
par(mfrow = c(2,2))

#fnbmd~ht
m1<-lm(fnbmd~wt)
plot(fnbmd~wt)
abline(m1)
summary(m1)

#fnbmd~ht
m2<-lm(fnbmd~ht)
plot(fnbmd~ht)
abline(m2)
summary(m2)
#fnbmd~age
m3<-lm(fnbmd~age)
plot(fnbmd~age)
abline(m3)
summary(m3)
#fnbmd~age+wt
m4<-lm(fnbmd~(age+wt))
plot(fnbmd~(age+wt))
abline(m4)
summary(m4)
#fnbmd~age+ht

#fnbmd~age+wt+ht
data
names(data)

