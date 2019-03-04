install.packages("xlsx")
install.packages("sqldf")
install.packages("reshape2")
install.packages("knitr")
install.packages("rpart")
install.packages("Hmisc")
install.packages("caret")
installed.packages("rpart.plot")
library(sqldf) #For SQL Functions
library(reshape2) #For plots
library(knitr) # For tables
library(Hmisc)# Description of the data
library(rpart)
library(caret)
library(rpart.plot)

library(xlsx)
salary_train <- read.csv("D://r workings//support vector machines//SalaryData_Train.csv")
salary_test <- read.csv("D://r workings//support vector machines//SalaryData_Test.csv")

View(salary_train)
View(salary_test)

data <- read.csv("D://r workings//support vector machines//SalaryData_Train.csv", header=FALSE, sep= ",", strip.white=TRUE,col.names= c("age", "workclass", "education", "educationnum", "maritalstatus", "occupation","relationship", "race", "sex", "capitalgain",
                                                                                    "capitalloss", "hoursperweek", "nativecountry",
                                                                                    "incomelevel"), na.strings= "?", stringsAsFactors = TRUE)
kable(head(data))

##data_train <- data.frame(salary_train,age ,workclass,education,educationno,maritalstatus,occupation,relationship,race,sex,capitalgain,capitalloss,hoursperweek,native,Salary)
##D <- as.factor(c("age","workclass","education","educationno","maritalstatus","occupation","relationship","race","sex","capitalgain","capitalloss","hoursperweek","native","Salary"))



# kvsm() function uses gaussian RBF kernel 

# Building model 

library(kernlab)
library(caret)
model1<-ksvm(Salary ~.,data = salary_train,kernel = "vanilladot")
model1

help(kvsm)
??kvsm

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(Salary ~.,data = salary_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=salary_test)
mean(pred_rfdot==salary_test$Salary) # 85.4 is better

# kernel = vanilladot
model_vanilla<-ksvm(Salary ~.,data = salary_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=salary_test)
mean(pred_vanilla==salary_test$Salary) #84.6


# kernal = besseldot
model_besseldot<-ksvm(Salary ~.,data = salary_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=salary_test)
mean(pred_bessel==salary_test$Salary) # 77

# kernel = polydot

model_poly<-ksvm(Salary ~.,data = salary_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = salary_test)
mean(pred_poly==salary_test$Salary) #84.6

################### forest fires ##########
forest<- read.csv("D://r workings//support vector machines//forestfires.csv")
View(forest)
scale(forest)
forest_train <- forest[1:360,3:31]
forest_test <- forest[361:517,3:31]
library(kernlab)
library(caret)
model1<-ksvm(size_category ~.,data = forest_train,kernel = "vanilladot")
model1

help(kvsm)
??kvsm

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(size_category ~.,data = forest_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=forest_test)
mean(pred_rfdot==forest_test$size_category) # 77

# kernel = vanilladot
model_vanilla<-ksvm(size_category ~.,data = forest_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=forest_test)
mean(pred_vanilla==forest_test$size_category) # 97.4 is better


# kernal = besseldot
model_besseldot<-ksvm(size_category ~.,data = forest_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=forest_test)
mean(pred_bessel==forest_test$size_category)# 67.5

# kernel = polydot

model_poly<-ksvm(size_category ~.,data = forest_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = forest_test)
mean(pred_poly==forest_test$size_category) # 97.45

