#Installing necessary packages and setting up working directory
library(glmnet)
library(klaR)
library(caret)
rm(list=ls())
setwd("C:/Users/Bajes01842/Google Drive/CS/Machine Learning/HW6/RPacks")
#setwd("C:/Users/Behrooz/Google Drive/CS/Machine Learning/HW6/RPacks")


#Reading in dataset 
AllD <- read.csv('default of credit card clients.csv',header = T,stringsAsFactors=FALSE)
AllD.matrix <- as.matrix(AllD) 

#Developing feature dataset
df <- matrix(as.double(AllD.matrix[,2:24]),ncol = 23) 

#Developing label dataset
dl <- AllD[,25]
dl.factor <- as.factor(AllD[,25])

#Creating the result matrix for storing the results of generalized linear model 
#with various ridge (α = 0), lasso (α = 1) and elastic net scenarios (0<α<1). 
results.matrix <- matrix(0, nrow = 4, ncol=12)
for(i in 1:11){
  print ((i-1)/10)
  cv.lasso = cv.glmnet(df,dl.factor,alpha = (i-1)/10, family="binomial", type.measure = 'class', nfolds=10)
  plot(cv.lasso)
  results.matrix[1,i] <- ((i-1)/10)
  results.matrix[3,i] <- min(cv.lasso[["cvm"]])
  results.matrix[2,i] <- cv.lasso[["lambda"]][which.min(cv.lasso[["cvm"]])]
  results.matrix[4,i] <- 1-min(cv.lasso[["cvm"]])
}

#Results of non-generalized linear model with lambda = 0 and α = 0.
cv.non.reg = cv.glmnet(df,dl.factor,alpha = 0, family="binomial", type.measure = 'class', lambda = c(0,0.001,0.1))
plot(cv.non.reg)
results.matrix[1,12] <- 0
results.matrix[3,12] <- min(cv.non.reg[["cvm"]])
results.matrix[2,12] <- cv.non.reg[["lambda"]][cv.non.reg[["cvm"]] == min(cv.non.reg[["cvm"]])]
results.matrix[4,12] <- 1-min(cv.non.reg[["cvm"]])

#Printing the results for regularized and regularized regression models
print(results.matrix)




