
#kiểm tra trong ds có phần tử nào <0 hay không
x<-c(-1,3,2,5)
if(any(x<0)){
  print("abx")
}else{
  print("zzz")
}

y<-c(8,3,-2,5)
ifelse(any(x<0),"y constains negative", "y contains all possitive")

#vòng lặp
for(i in 1000:2016){
  output<-paste("the year is: ",i)
  print(output)
}

x<-NULL
for (i in 2010:2016) {
  output<- paste("the year is: ",i)
  x<-append(x,output)
}
x

counter<-1
x<-5
set.seed(1) #genelization random identical
while(x>=3 & x<=8){
  coin<-rbinom(1,1,0.5) #find 1 random values from a sample of 1 with probability of 0.5
  print(coin)

  if(coin == 1){
    x<-x+1
  }else{
    x<-x-1
  }
  cat("On iteration", counter,", x=",x,"\n")
  counter<-counter+1
}

#create a function
bp<-function(a){
  b<-0
  for (i in 1:a) {
    b<-b+(i^2)
  }
  print(b)
}
bp(5)

#tinh giai thua = de quy
giaithua<-function(n){
  if(n==1){ return(1)}
  
  return( n * giaithua(n-1))
}
k <- 3
cat("giai thua cua ",k," la: ",giaithua(k))

#tính giai thừa bằng vòng lặp

gt<-function(n){
 gt<-1
  for (i in 1:n) {
    gt<-gt*i
  }
  return(gt)
}
k<-3
cat("giai thua cua",k,"la:", gt(k))




