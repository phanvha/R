



#   Note: save file with fullname (ex. NguyenVanHung)

# Exercise 1.  read dataset advertising.csv 

dt = read.csv(file="D:\\PhanVanha\\advertising.csv",header = TRUE)
dt
edit(dt)
names(dt)

# 1. Eliminate the first two columns of the advertising data frame. 
dt1<-dt[,3:5]
names(dt1)

# 2. What type of variables does the data frame contain? 
str(dt1)
 #dữ liệu gồm 3 biến: radio, newspaper, sales. tất cả điều là kiểu Num


# 3. Compute the mean of the variable TV. 
mtv<-dt[,c("TV")]
mtv
mean(mtv)

# 4. Compute the standard deviation of the variable Sales.
sd(dt$sales)
sd

# 5. Print the summary statistics for this data frame. 
summary(dt)

# 6. Create a new variable called SalesBin in the data frame that gets the value 1 if the sales are above 14 and zero otherwise.
dt$SalesBin <- ifelse(dt$sales>14, 1, 0)
View(dt)

# Exercise 2
# 1. Create the variables:  a = {1, 1.3, 1.6, 2, 3.5} 		b = 2 + 3a 
a <- c(1, 1.3, 1.6, 2, 3.5)
a
b <- c(2 + 3*a)
b
# 2. Create a scatterplot of a and b with the name "Linear function", axes labels "a - explanatory variable" and "b - response variable". Color the points in green. 
plot(a,b,type = "o", col = "green", xlab = "a - explanatory variable", ylab = "b - response variable",
     main = "Linear function")
# 3. Add a line through the points using the lines() low level plotting function.
model <- lm(b~a)
abline(model)

# Exercise 3
# 1. Run the following command in R and assign it to an object z1: sample(1:4, 20, replace = TRUE) 
z1 <- sample(1:4, 20, replace = TRUE)
z1

# 2. Compute the frequencies for this vector by using the function table().
table(z1)

# 3. Draw a bar chart of the absolute frequencies of the vector z1. Use a different color for each bar.
z1.fre = table(z1)
color=c("red","yellow","blue","green")
barplot(z1.fre,col=color)
# Exercise 4.
# 1. Create a contingency table for the variables NewspaperCat and SalesBin in the advertising data set using the table(x, y) function. 
ns<-data.frame("NewspaperCat"= dt$newspaper,"SaleBin"= dt$SalesBin)
View(ns)
# 2. Produce a spineplot. 
spineplot(table(ns$dt.newspaper))
# 3. Create a contingency table for the variables NewspaperCat and SalesBin in the advertising data set using the table(x, y) function. 
# 4. Produce a spineplot.
# Exercise 5
# 1. Fit a simple regression model for Sales ~ Radio and Sales ~ Newspaper 
# 2. Produce the scatterplot together with regression line for both models.
# 3. Fit the regression model for Sales ~ TV + Radio and save it as an object named fitTVRadio.
# 
# Exercise 6.  read data set credit.csv
cre = read.csv(file="D:\\PhanVanha\\credit.csv",header = TRUE)
cre
edit(cre)
names(cre)

# 1. A histogram of the balance variable 
hist(cre$Balance)

# 2. A boxplot of the income variable 
boxplot(cre$Income)
# 3. A barplot of the absolute frequencies of the student variable 
td<- table(cre$Student)
barplot(td)
# 4. A spineplot showing the distribution of the student for each default class (No/Yes)

spineplot(sp)
# 5. For the credit data set perform the model selection procedure using the step() function. 
mdl<-lm(cre$Income ~ ., data = cre)
step(mdl, direction = "both")

# 6. Print the summary of the results. Which model has been chosen?
summary(mdl)
mdl_cls = lm(cre$Income ~ cre$Limit+cre$Rating+cre$Cards+cre$Student+cre$Balance)
mdl_cls
#   
rm(list = ls())
