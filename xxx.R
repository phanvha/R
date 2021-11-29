A<- c(8,9,11,4,7,8,5)
B<- c(7,17,10,14,12,24,11,22)
C<- c(28,21,26,11,24,19)
D<- c(26,16,13,12,9,10,11,17,15)


maxlength <-max(length(A),length(B),length(C),length(D))
maxlength


A1<- c(A,rep(NA,maxlength -length(A)))
A1
B1<- c(B,rep(NA,maxlength -length(B)))
B1
C1<- c(C,rep(NA,maxlength -length(C)))
C1
D1<- c(D,rep(NA,maxlength -length(D)))
D1

datax<-data.frame(A=A1,B=B1,C=C1,D=D1)
datax
edit(datax)

ma<-mean(A)
ma
mb<-mean(B)
mb
mc<-mean(C)
mc
md<-mean(D)
md

x<-c(A,B,C,D)
x
ave<-mean(x)



ssb<-(length(A)*(ma-ave)^2)+(length(B)*(mb-ave)^2)+(length(C)*(mc-ave)^2)+(length(D)*(md-ave)^2)
ssb


#tinhs sum square with group variation
#cách 1:
sqr = function(x){
  return(x*x)
}
sswa <-sum(sqr(A-ma))
sswb <-sum(sqr(B-mb))
sswc <-sum(sqr(C-mc))
sswd <-sum(sqr(D-md))
ssw <- sswa+sswb+sswc+sswd
ssw

#cách 2:
sswa <- 0
for i in A:
  sswa <- sswa+(i-mb)^2
sswa

#mean square
MS = sumsquare(ss)/degreefreedom(df)#bat tụ do giữa 4 nhóm là 4-1=3
#MSB giữa 4 nhóm
msb = ssb/3
msb
msw = ssw/26
msw
#SSW cho 26 nhóm(30 phần tử -4 nhóm)
#f-value = msb/msw

library()
#############################
group <- c(rep("A", length(A)), 
           rep("B", length(B)),
           rep("C", length(C)),
           rep("D", length(D)))
group
data<-data.frame(x,group)
data
boxplot(x~group, col = "blue")
av = aov(x~group)
summary(av)
qqnorm(x)


TukeyHSD(av)
plot(TukeyHSD(av))
