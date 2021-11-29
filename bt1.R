#check location
getwd();
#set to working
setwd("D\\PhanVanHa")
getwd()
data()
# reading dataframe/data set iris
df<- iris
# xem có bao nhiêu dòng, cột
dim(df)

# xem tên cột (variables)
names(df)

#xem kiểu dữ liệu
str(df)

# xem toàn bộ dữ liệu
View(df)
edit(df)
head(df)

# ---------------
datasets[row,column]
df1 <- df[,-5] # xoa cot 1
df1
names(df1)
rm()

df[,]# all rows and all columns
df[1,]# dong 1
df[1:2,] #dong 1+2
df[5:10,]
df[c(1,3,150),]
df[1,2:3]
df[1:2,2:3]
seq(df)
df[seq(),]



#list rows old
t <- df
edit(t)
new <-na.omit(df)


#create dataframe
d = data.frame(Name=c('Ho Nhat Huy','Phan Van Ha','Nguyen Canh Thong'),
               index=c('Danang','Nghe An','Nghe An'),
               Age=c(21,21,22))
d
# ------------
new <- na.omit(d)# xóa các data lỗi
new
names(df)
dim(df)
# phân nhóm
table(df$Species)
aggregate(df$Sepal.Length, by=list(df$Species),FUN=sum)
#method2
library(dplyr)
df %>% group_by(Species) %>% summarise(Sepal.Length-n())
#tuong tu tim min, max, mean, sum cua tung nhom

#sort the column in df
attach(df)
df2 <-df[order(Sepal.Length),] #tang
df2
df3 <-df[order(-df$Sepal.Length),]#giam
df3
df3 <-df[order(df$Sepal.Length),-df$Sepal.Width]#giam
df3

sapply(df,class)# get th list of column and its datatype 
str(df)# get th list of column and its datatype 
names(df)

#rename all the column
d<-df

colnames(d) <- c("c1","c2","c3","c4","c5")
names(d)
d
names(d)[1]<-"column1"
d
names(d)
d
names(d)[names(d)=="c5"]<-"column5"
names(d)

#replace missing value with zero
df
df$Sepal.Length[is.na(df$Sepal.Length)] <-0
df
# replace missing value of column with mean
df
df$Sepal.Length[is.na(df$Sepal.Length)] <- mean(df$Sepal.Length, na.rm = TRUE)
df
# replace missing value of column with median
df
df$Sepal.Length[is.na(df$Sepal.Length)] <- median(df$Sepal.Length, na.rm = TRUE)
df


#filter data
library(dplyr)
result_or<-df %>% filter(Sepal.Length>7.5 | Sepal.Width>3.8 | Petal.Length>6.4)
result_or
result_and<-df %>% filter(Sepal.Length>7.5 & Sepal.Width>3.5)
result_and

#summarise
summarise(df,Sepal.Length_mean=mean(Sepal.Length),Sepal.Width_median=median(Sepal.Width),Petal.Length_max=max(Petal.Length))
summary(df)
