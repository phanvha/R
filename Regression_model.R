#đây là một dataset thống kê về các chỉ số liên quan đến tác động của sử dụng súng qua các năm(tại Mỹ)
#dữ liệu bao gồm 1173 dòng và 14 cột tương ứng 14 biến
#các biến bao gồm:
  #state: địa điểm
  #year: năm
  #violent: tỉ lệ tội phạm bạo lực(tính trên 100000 người)
  #murder: tỉ lệ giết người
  #robbery: tỉ lệ giết người
  #prisoners: tỉ lệ tội phạm bị giam giữ
  #afam: phần trăm dân số tiểu bang là người Mỹ gốc Phi, tuổi từ 10 đến 64
  #cauc: phần trăm dân số tiểu bang là người da trắng, tuổi từ 10 đến 64
  #male: phần trăm dân số bang là nam giới, tuổi từ 10 đến 29
  #population: dân số bang, hàng triệu người
  #income: thu nhập thực tế bình quân đầu người của tiểu bang (đô la Mỹ)
  #density: dân số trên một dặm vuông diện tích đất, chia cho 1.000

dat = read.csv(file="D:\\PhanVanha\\X-Guns.csv",header = TRUE)
dat
dim(dat)
names(dat)
edit(dat)

dt<-dat
dt<-na.omit(dt) #làm sạch dữ liệu
dt1<-dt[,2:12] #bỏ đi các cột không chứa các giá trị là số
names(dt1)
edit(dt1)
attach(dt1)

#tạo biểu dồ tương quan giữa các biến

#kiểm tra mối quan hệ giữa các biến độc lập của bạn và đảm bảo rằng chúng không có mối tương quan quá cao.
cor(dt1$year,dt1$violent)

#xem tổng quan mối quan hệ giữa các biến trong dataframe
#vẽ biểu đồ phân tán giữa các biến
pairs(dt1) 
pairs(~year+violent+murder+robbery+prisoners)# sử dụng lệnh này nếu chỉ muốn xem mối quan hệ của 5 biến

#mối quan hệ tuyến tính giữa năm và tỉ lệ tội phạm bạo lực 
year.violent.lm <- lm(year ~ violent)
plot(year ~ violent) #vẽ biểu đồ tuyến tính giữa năm và tỉ lệ bạo lực
abline(year.violent.lm) #vẽ đường thẳng
summary(year.violent.lm)

#mối quan hệ tuyến tính giữa tỉ lệ giết người theo từng năm
year.murder.lm <- lm(year ~ murder)
plot(year ~ murder)
abline(year.murder.lm)
summary(year.murder.lm)

#mối quan hệ tuyến tính giữa tỉ lệ cướp giật theo từng năm
year.robbery.lm <- lm(year ~ robbery)
plot(year ~ robbery)
abline(year.robbery.lm)
summary(year.robbery.lm)

#mối quan hệ tuyến tính giữa tỉ lệ giam giữ theo từng năm
year.prisoners.lm <- lm(year ~ prisoners)
plot(year ~ prisoners)
abline(year.prisoners.lm)
summary(year.prisoners.lm)

#ma trận tương quan giữa các biến "year","violent","murder","robbery","prisoners"
cor(dt1[,c("year","violent","murder","robbery","prisoners")])


#thực hiện chia bộ dữ liệu thành 2 phần: 75% dùng để xây dựng mô hình, 25% dùng để quan sát kiểm định
#sử dụng thư viện caTools
library("caTools")
dt1["ID"] = c(1:1173)
set.seed(123)
split = sample.split(dt1$ID, SplitRatio = 2/3)

mxd = subset(dt1,split==TRUE)
mkd = subset(dt1,split==FALSE)

#xây dựng mô hình 1
#sử dụng biến year, violent, murder, robbery để dự báo số tù nhân bị giam giữ
mohinh1 <- lm(prisoners ~ year+violent+murder+robbery)
mohinh1
summary(mohinh1)

#dự báo giá trị của prisoners trong mẫu xây dựng, sử dụng hàm predict
db.mxd = predict(mohinh1, mxd)
#tổng phần dư bình phương(RSS) của mẫu xây dựng
RSS.xd = sum((db.mxd - mxd$prisoners)^2)

#tổng phần dư bình phương của mô hình cơ sở
TSS.xd = sum((mean(mxd$prisoners)- mxd$prisoners)^2)

#giá trị R-square(R2) của mô hình trên mẫu xây dựng
R2.xd = 1 - (RSS.xd/TSS.xd)

#kết quả R2 = 0.79, kết quả này bằng với khi ta tính bằng cách sử dụng hàm summary ở trên

#tương tự, tính R2 cho mẫu kiểm định
#khi sử dụng TSS cho mẫu kiểm định, ta sử dụng trung bình median của biến prinoners
#(nhằm không sử dụng thông tin của mẫu xây dựng)
db.kd = predict(mohinh1, mkd)
RSS.kd = sum((db.kd - mkd$prisoners)^2)
TSS.kd = sum((median(mxd$prisoners)-mkd$prisoners)^2)
R2.kd = 1 - (RSS.kd/TSS.kd)

#################
#Tiếp tục xây dựng mô hình 2: sử dụng tất cả các biến trong quá trình dự báo

edit(mxd)
mohinh2 = lm(prisoners ~ ., data = mxd)
summary(mohinh2)

#dự báo giá trị R2 cho mẫu xây dựng, 
db.mxd = predict(mohinh2, mxd)
RSS.xd = sum((db.mxd - mxd$prisoners)^2)
TSS.xd = sum((mean(mxd$prisoners)- mxd$prisoners)^2)
R2.xd = 1 - (RSS.xd/TSS.xd)

#tương tự, tính R2 cho mẫu kiểm định
db.kd = predict(mohinh2, mkd)
RSS.kd = sum((db.kd - mkd$prisoners)^2)
TSS.kd = sum((median(mxd$prisoners)-mkd$prisoners)^2)
R2.kd = 1 - (RSS.kd/TSS.kd)

#kết quả: # R2.kd = 0.8302, lớn hơn một chút so với R2.xd = 0.8066











