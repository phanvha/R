# doc du lieu tu internet

getwd() #display path

setwd() #setting path

PATH <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv"
df <- read.csv(PATH, sep = ",")

# dung cac lenh R da hoc de kiem tra df(dataset)

dim(df) # xem so hang cot

nrow(df)
ncol(df)

names(df) # xem ten
str(df) # xem kieu du lieu cua bien

sapply(df, class)

# 2 lenh xem du lieu
View(df)
edit(df)

str(df)

df$Pclass <- as.factor(df$Pclass) # muon phan nhom du lieu thì dung factor

str(df)

df[,] # all rows and columns

# lay mau ngau nhien thuong dung trong thong ke

df[sample(nrow(df),2),]

t1 <- df[sample(nrow(df1),12),]
t1

# loai bo 1 cot nao do
# tao 1 cai data moi dataframe with (1,2,4,5,10,11)

df1 <- df[,c(1,2,4,5,10,11)] # lay cot nao thi ghi cot do vao c(...)
df1
str(df1)
t1 <- df1[sample(nrow(df),5),]
t1

# loc du lieu
library(dplyr)
result_or <- df1 %>% filter(df1$Pclass =="1" |df1$Age >= 40.0)
result_or

#request: Pclass == 2  and Sex = female
re1 <- df1 %>% filter(df1$Pclass =="2" & df1$Sex =="female")
re1
#          Pclass == 3 and male
re2 <- df1 %>% filter(df1$Pclass =="3" & df1$Sex =="male")
re2 
#          Pclass == 1 or 2 and male
re3 <- df1 %>% filter((df1$Pclass=="1" | df1$Pclass =="2") & df1$Sex =="male")
re3
#          Pclass 1 and female and Age > 50
re4 <- df1 %>% filter(df1$Pclass =="1" & df1$Sex =="female" & df1$Age>50)
re4

# noi 2 csdl theo chieu doc vaf chieu ngang dataset 
# chuan bi du lieu
df

#d1: lay ra 5 dong ngau nhien tu df
d1 <- df[sample(nrow(df),5),]
d1
#d2: lay ra 2 cot 10 va 11 doi ten cot 10 = new1, cot 11 = new2
d2 <- df[,c(10,11)]
d2
#noi df va d1(noi hang) -> dfd1
dfd1 <- rbind(df,d1)
dim(dfd1)
# noi df va dfd1 (noi theo cot) -> final
dfd2 <- cbind(df,d2)
dim(dfd2)
str(dfd2)
# noi d2 va df
names(d2)
colnames(d2) <- c("new1","new2") #doi ten
names(d2)

d1d2<cbind(df,d2)

str(dfd2)

#check information for join column
dim(result_join_column)


#note cbind() must same number rows


s<-data.frame(x=1:5,math =c(2,3,4,9,8))
s
s2<-data.frame(x=3:10,science=c(2,3,4,5,3,7,8,5))
s2

#neu so dong khac nhau thif k dung dc
ss<- cbind(s,s2)#error

merge(s,s2)
merge(s,s2,by="x")

#merge data frames by column name
merge(s,s2,by="x",all.x = TRUE)#keep all rows of x
merge(s,s2,by="x",all.y = TRUE)#keep all rows of x
ss = merge(s,s2,by="x",all.x = TRUE,all.y = TRUE)#keep all rows of x,y
ss
ss$math[is.na(ss$math)] <- median(ss$math, na.rm = TRUE)
ss
ss$science[is.na(ss$science)] <- median(ss$science, na.rm = TRUE)
ss
ss1 <- ss[sample(nrow(ss),3),]
sss = rbind(ss,ss1)
sss
ss
#lấy 3 thằng ngẫu nhiên của ss nối vào ss -> sss
ss1 <- ss[sample(nrow(ss),3),]
sss = rbind(ss,ss1)
sss

#xử lý dữ liệu đã mất(thiếu) trên datasets ss 
#----------
#remove duplicate rows of the datasets
t1 <- unique(ss)
dim(t1)

t2<- sss[!duplicated(sss),] #duplicated cho  biết những bản ghi giống nhau
t2

library(dplyr)
# cách khác để quan sát các dòng giống nhau
t3 <- distinct(ss)
t3

#remove rows duplicated in a specific column
#tạo một dataset mới có cấu trúc giống cấu trúc của ss và có 2 dòng dữ liệu()
newdata = data.frame(x=11:12,math =c(9,5),science=c(4,10))
newdata

d3 = rbind(ss,newdata)
d3

t4 <- distinct(d3, math, .keep_all = TRUE)
t4
t5 <- d3 %>% distinct(science, .keep_all = TRUE)
t5
ss

#delete all ls()
rm()#xoas một biến
rm(list = ls())#xóa toàn bộ biến tạm


#yêu cầu btvn:
#-tìm một datasets/dataframes trên internet(.csv, txt, xls, xlsx) -> download và đọc file vào R
#-kiểm tra dataframe->dòng, cột, kiểu dữ liệu,...
#-thống kê dữ liệu bị thiếu/mất
#-làm sạch -> loại bỏ dữ liệu bị thiếu, các dòng giống nhau
#-lọc dữ liệu theo các điều kiện(tự chọn)

#bài làm

#Source: https://www.kaggle.com/spscientist/students-performance-in-exams?select=StudentsPerformance.csv

bm = read.csv(file="D:\\PhanVanha\\Students.csv",header = TRUE) # đọc file
names(bm)
bm
dim(bm) #rows = 1000 and columns = 8
bm1 <- bm[sample(nrow(bm),2),] #lấy ngẫu nhiên 2 dòng
bm1
#nối bm1 vào bm -> bm2
bm2 <- rbind(bm,bm1)
dim(bm2)

# xóa các rows giống nhau
bm3 <- unique(bm2)
dim(bm3)

#kiểu dữ liệu
str(bm)

# tạo một datasets mới từ bm
new_bm<-bm
new_bm
names(new_bm)
#lấy tất cả rows là male
bm_male <- new_bm %>% filter(gender=="male") 
bm_male
#lấy tất cả các rows là male và có điểm toán > 80
math_score<- new_bm %>% filter(new_bm$math.score >80)
math_score
bm_math_of_males <- math_score %>% filter(gender=="male")
bm_math_of_males
dim(bm_math_of_males)# có 108 rows là male và điểm toán > 80

#lấy tất cả rows là female
bm_female <- new_bm %>% filter(gender=="female") 
bm_female
#lấy tất cả các rows là female và có điểm đọc < 70
reading_score<- new_bm %>% filter(new_bm$reading.score < 70)
reading_score
bm_reading_of_females <- reading_score %>% filter(gender=="female")
bm_reading_of_females
dim(bm_reading_of_females)# có 200 rows là female và điểm đọc < 70

#lấy tất cả rows có điểm toán > 50, điểm viết > 50 và điểm đọc > 50
bm_z = new_bm %>% filter(new_bm$reading.score > 50 & new_bm$math.score>50 & new_bm$writing.score >50)
bm_z


