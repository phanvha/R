dt = read.csv(file="D:\\PhanVanha\\dataset3.csv",header = TRUE)
dt
names(dt)

# tạo dataset gồm 4 biến từ x1 -> x4 vàbieensn phụ thuộc y


d1<-dt[,-c(7,8)]
d1
names(d1)
#tạo các mô hình vơi các biến độc lập(tập con của dataset)

#hiển thị thông tin của từng mô hình
m1<-lm(y~x1, data = d1)
summary(m1)

m2<-lm(y~x2, data = d1)
summary(m2)

m3<-lm(y~x3, data = d1)
summary(m3)

m4<-lm(y~x4, data = d1)
summary(m4)

m7<-lm(y~x7, data = d1)
summary(m7)
pairs(d1)

md = lm(y~x1+x2+x3+x4+x5+x6+x7, data = dt)
summary(md)
pairs(dt)


modelx6<- lm(y~x6, data = dt)
summary(modelx6)

modelAll<-lm(y~.,data = dt)
step(modelAll, direction = "both")

id<-1:19
conc <- c(1.0,1.5,2.0,1.0,4.0,4.5,5.0,)

strenght<-c()
  
dm<-data.frame(id,conc, strenght)
dm
linear<-lm(strenght~conc)
summary(linear)
plot(strenght~conc,xlap="Concentration of hardwood", ylap="tensile strenght")



#bài tập về nhà
  # mỗi SV tìm một bộ dữ liệu(dataset), càng lớn càng tốt(tối thiểu 10 cột, 1000 dòng)
  #tìm mô hình hồi quy tuyến tính tối ưu
  #giải thích các tham số
  #hiển thị và giải thích ý nghĩa các tham số của mô hình
  #thời hạn: 1 tuần
  #báo cáo trươc lớp








