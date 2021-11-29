dat = read.csv(file="D:\\PhanVanha\\OJ.csv",header = TRUE)
dat
dim(dat)
names(dat)
edit(dat)

dt <- dat[1:300,-c(2)]#test trên 300 dòng đầu tiên
edit(dt)




modelAll<-lm(PriceCH~.,data = dt) #tìm mô hình phù hợp nhất
step(modelAll, direction = "both")
attach(dt)

m1<-lm(PriceCH~.)
plot(PriceCH~., data = dt)
abline(m1)
summary(m1)
pairs(dt)

library("MASS")
str(Boston)


