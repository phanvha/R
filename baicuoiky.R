dt = read.csv(file="D:\\PhanVanHa\\Housing.csv",header = TRUE)
dt
#dữ liệu gồm 546 hàng và 13 cột
#x : số thứ tự 
#price : giá bán của căn nhà
#lotsize : bedrooms
#bedrooms : số lượng phòng ngủ
#bathrms : số phòng tắm đầy đủ tiện nghi
#stories : số tầng
#driveway : nhà có đường lái xe hay không ?
#recroom nhà có phòng giải trí  hay không?
#gashw : nhà có sử dụng gas để nấu hay không?
##có máy lạnh trung tâm hay không?
#garagepl: số lượng nhà để xe
#prefarea : nhà có ở khu phố yêu thích hay không?


names(dt) #xem tên của các cột trong bảng
dim(dt) #xem số lượng hàng, cột
str(dt) #xem kiểu dữ liệu
head(dt)
edit(dt) #xem tổng quan dữ liệu
View(dt)


#########################
#làm sạch dữ liệu
dt <- na.omit(dt)
dt

#######################
#thống kê chi tiết

#thống kê các thông số cơ bản của các trường dữ liệu, bao gồm: trung bình, min, max, median, các giá trị tới hạn của phân vị tới 1, 3
summary(dt)


##########################
#trích xuất dữ liệu
dt[,2] #lấy cột thứ 2 của dataframe
dt[1,] #lấy dòng thứ nhất của dataframe
dt[3,2] #lấy phần tử dòng thứ 3, cột thứ 2

#filter data
attach(dt)
library(dplyr)
result_or<- dt %>% filter(dt$price > 50000) #giá nhà lớn hơn 50000
result_or

View(result_or)
result_a<-dt %>% filter(dt$price > 20000 & dt$lotsize>4000 ) #giá nhà < 20000 và diện tích đất nền > 4000 feet
result_a
result_b<-dt %>% filter(dt$bathrms > 1)
result_b

price.max <- summarise(dt,price_max=max(price)) #tìm giá nhà đắt nhất
price.max
price.min <- summarise(dt,price_min=min(price)) #tìm giá nhà rẻ nhất
price.min

bedrooms.max <- summarise(dt,bedrooms_max=max(bedrooms)) #tìm giá nhà đắt nhất
bedrooms.max




#thêm một dòng vào dataframe
names(dt)
dim(dt)
dt <- rbind(dt,c(547,100000,9000,5,2,2,"yes","yes","yes", "yes","yes",2,"yes"))
dt
dt[547,]
dim(dt)
#xóa một dòng dữ liệu trong dataframe
dt[547,]
dim(dt)
dt[-547,]


###############
#Biến đổi dữ liệu
#Sắp xếp dữ liệu cột price tăng dần
dt2 <-dt[order(price),] #tang
dt2

#sắp xếp dũ liệu theo cột diện tích lô đất lotsize giảm dần
dt2 <-dt[order(-dt$lotsize),] #giảm dần
dt2
edit(dt2)

#xóa các cột dữ liệu không phải là số
df<-dt[ , -c(1,7:11,13) ]
names(df)
edit(df)


#tạo các biểu đồ
attach(df)
df

#kiểm tra mối quan hệ giữa các biến độc lập của bạn và đảm bảo rằng chúng không có mối tương quan quá cao.
cor(df$price,df$lotsize)
cor(df$price,df$bedrooms)
cor(df$price,df$bathrms)
cor(df$price,df$stories)
cor(df$price,df$garagepl)

#vẽ biểu đổ phân tán
xd <- data.frame(price,bedrooms,bathrms, stories,garagepl)
pairs.default(xd, pch=18) 
pairs(~price+lotsize)

#tạo biểu đồ cột hiển thị tổng quan giá trị của cột bedrooms
beds <- table(bedrooms)
barplot(beds, main ="bedrooms") # Hien thi bieu do cot 

#tạo biểu đồ tròn cho cột bedrooms
pie(table(bedrooms), main = "bedrooms")


#mối quan hệ tuyến tính giữa giá nhà và diện tích đất nền
price.lotsize.lm <- lm(price ~ lotsize)
plot(price ~ lotsize) #vẽ biểu đồ tuyến tính giữa giá nhà và diện tích đất nền của căn nhà
abline(price.lotsize.lm) #vẽ đường thẳng
summary(price.lotsize.lm)
### ta thấy rằng nếu như diện tích đất nền càng lớn thì giá của căn nhà sẽ càng cao

#mối quan hệ tuyến tính giữa giá nhà và diện tích đất nền
price.lotsize.lm <- lm(price ~ lotsize)
plot(price ~ lotsize) #vẽ biểu đồ tuyến tính giữa giá nhà và diện tích đất nền của căn nhà
abline(price.lotsize.lm) #vẽ đường thẳng
summary(price.lotsize.lm)
### dựa vào biểu đồ, ta thấy rằng nếu như diện tích đất nền càng lớn thì giá của căn nhà sẽ càng cao

#mối quan hệ tuyến tính giữa giá nhà và số lượng phòng ngủ
price.bedsrooms.lm <- lm(price ~ bedrooms)
plot(price ~ bedrooms) #vẽ biểu đồ tuyến tính giữa giá nhà và số lượng phòng ngủ của căn phòng đó
abline(price.bedsrooms.lm) #vẽ đường thẳng
summary(price.bedsrooms.lm)
### dựa vào biểu đồ, ta thấy rằng nếu như nhà có số lượng phòng ngủ càng lớn thì giá của căn nhà có càng cao, 

#mối quan hệ tuyến tính giữa giá nhà và số lượng phòng tắm đầy đủ tiện nghi
price.bathrms.lm <- lm(price ~ bathrms)
plot(price ~ bathrms) #vẽ biểu đồ tuyến tính giữa giá nhà và diện tích đất nền của căn nhà
abline(price.bathrms.lm) #vẽ đường thẳng
summary(price.bathrms.lm)
boxplot(price~bathrms)
### dựa vào biểu đồ, ta thấy rằng nếu số lượng phòng tắm càng nhiều thì giá của căn nhà sẽ càng cao

#thực hiện tương tự mối tương quan giữa giá của căn nhà tới số tầng của căn nhà và số lượng nhà để xe,
#cũng cho ra kết quả giá nhà sẽ có tỉ lệ thuận với số lượng của số tầng và nhà để xe.
#hay nói cách khác, giá của căn nhà sẽ phụ thuộc vào số lượng của các thiết bị, các phòng,...của căn nhà.


#################
#Xây dựng các mô hình
#phân tích dữ liệu
#Xây dựng mô hình hồi quy tuyến tính cho bộ dữ liệu

#thực hiện chia bộ dữ liệu thành 2 phần: 75% dùng để xây dựng mô hình, 25% dùng để quan sát kiểm định
#sử dụng thư viện caTools
dim(df)
library("caTools")
df["ID"] = c(1:546)
set.seed(123)
split = sample.split(df$ID, SplitRatio = 2/3)

mxd = subset(df,split==TRUE)
mkd = subset(df,split==FALSE)

#xây dựng mô hình 1
#sử dụng biến price, lotsize, bedsrooms, bathrms, stories, garagel để sự đoán tự tăng giá của căn nhà
mohinh1 <- lm(price ~ bedrooms+bathrms+stories+garagepl)
mohinh1
step(mohinh1)
summary(mohinh1)

#dự báo giá trị của price trong mẫu xây dựng, sử dụng hàm predict
db.mxd = predict(mohinh1, mxd)
#tổng phần dư bình phương(RSS) của mẫu xây dựng
RSS.xd = sum((db.mxd - mxd$price)^2)

#tổng phần dư bình phương của mô hình cơ sở
TSS.xd = sum((mean(mxd$price)- mxd$price)^2)

#giá trị R-square(R2) của mô hình trên mẫu xây dựng
R2.xd = 1 - (RSS.xd/TSS.xd)

#kết quả R2.xd = 0.44, kết quả này bằng với khi ta tính bằng cách sử dụng hàm summary ở trên

#tương tự, tính R2 cho mẫu kiểm định
#khi sử dụng TSS cho mẫu kiểm định, ta sử dụng trung bình median của biến prices
#(nhằm không sử dụng thông tin của mẫu xây dựng)
db.kd = predict(mohinh1, mkd)
RSS.kd = sum((db.kd - mkd$price)^2)
TSS.kd = sum((median(mxd$price)-mkd$price)^2)
R2.kd = 1 - (RSS.kd/TSS.kd)

#kết quả R2.kd = 0.45, kết quả này cao hơn một chú
#so với khi ta tính bằng mẫu xây dựng ở trên(khoảng 0.01)

#################
#Tiếp tục xây dựng mô hình 2: sử dụng tất cả các biến trong quá trình dự báo

edit(mxd)
mohinh2 = lm(price ~ ., data = mxd)
summary(mohinh2)

#dự báo giá trị R2 cho mẫu xây dựng, 
db.mxd = predict(mohinh2, mxd)
RSS.xd = sum((db.mxd - mxd$price)^2)
TSS.xd = sum((mean(mxd$price)- mxd$price)^2)
R2.xd = 1 - (RSS.xd/TSS.xd)

#kết quả R2.xd = 0.57

#tương tự, tính R2 cho mẫu kiểm định
db.kd = predict(mohinh2, mkd)
RSS.kd = sum((db.kd - mkd$price)^2)
TSS.kd = sum((median(mxd$price)-mkd$price)^2)
R2.kd = 1 - (RSS.kd/TSS.kd)

#kết quả: # R2.kd = 0.61, lớn hơn một chút so với R2.xd = 0.57



#################
#xây dựng mô hình phương sai(Analysis Variance)
#trích xuất dữ liệu
house <- c(rep("price",546),rep("lotsize",546),rep("bedsrooms",546),rep("bathrms",546),rep("stories",546),rep("garagepl",546))
house

x <- c(df$price, df$lotsize, df$bedrooms, df$bathrms, df$stories, df$garagepl)

df1 <- data.frame(house, x) #tạo một dataframe chứa 2 cột: house và x
df1
View(df1)
boxplot(x ~ house, data=df)
#tính phương sai
aav <- aov(x ~ house, data = df1)
aav
summary(aav)


rm(list=ls())

