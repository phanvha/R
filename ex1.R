getwd()
setwd("D:\\PhanVanHa")
getwd()

df<-iris
df
dim(df)
edit(df)

df1<-df[,-5]
df1
names(df1)
names(df)
df1[,2]


t1 <- df1[sample()]


result_all<-df %>% filter(Pclass =="3" & Sex == "female" & Age >= 40)
result_all


#request: filter data:
#Pclass 2 and female
#Pclass 3 and male
#Pclass 1 or 2 and male
#class 1 or 2 and female and Age > 50

#-------------
# ý tưởng: nối 2 cơ sở dữ liệu theo chiều ngang và chiều dọc
# chuẩn bị dữ liệu(dataset)
d1<-df[sample(nrow(df),5),]
d1
#d1 lấy 5 dòng ngẫu nhiên từ df
#d2: lấy 2 cột 10 và 11 , đổi tên c10 = new1, c11 = new2
#nối hàng: df vào d1()-> dfd1
dfd1<-rbind(df,d1)
dim(dfd1)

#nối df và dfd1(theo cột)-> final
#nối d2 và df
d2 

names(d2)

colnames(d2)<-c("new1","new2")
names(d2)

d1d2<cbind(df,d2)

str(dfd2)

#check information for hion column
dim(result_join_column)


#note cbind() must same number rows


s<-data.frame(x=1:5,math =c(2,3,4,9,8))
s
s2<-data.frame(x=3:10,science=c(2,3,4,5,3,7,8,5))
s2

merge(s,s2)
merge(s,s2,by="x")




