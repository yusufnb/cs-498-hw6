#made with guidance from Study Group (Olga, Shairoz, RJ, and teammates Yusuf + Behrooz), as well as hkiang01 https://github.com/hkiang01/Applied-Machine-Learning/blob/master/assignment6/regression.R
#setwd("~/Dev/CS498/HW6/cs-498-hw6")
# setwd to source file location using R Studio: Session -> Set Working Directory -> To Source File Loc
library('stats') 
library('MASS')

data<-read.csv('default_plus_chromatic_features_1059_tracks.txt', header=FALSE, sep = ',')
colnames(data)[117] <- "lat"
colnames(data)[118] <- "long"
x_data <-data[,-c(ncol(data),ncol(data)-1)]
y_data <-data[,c(ncol(data)-1,ncol(data))]
y_lat <- y_data[,c(1)]
y_long <- y_data[,c(2)]
#First, build a straightforward linear regression of latitude (resp. longitude) against features.
#What is the R-squared? Plot a graph evaluating each regression.
x_matrix <- as.matrix(x_data)
y_lat_matrix <-as.matrix(y_lat)
xy_lat_matrix <-data.frame(ind=x_matrix[,c(1:ncol(x_matrix))], dep=y_lat_matrix[,c(1)])
xy_lat_matrix.lm<-lm(dep~x_matrix,data=xy_lat_matrix) 

#predict latitude
pred_lat<-xy_lat_matrix.lm$fitted.values
obs_lat<-y_lat_matrix
#res_lat<-xy_lat_matrix.lm$residuals
plot(x=obs_lat, y=pred_lat, type="p", main="Predicted vs. Observed Latitude", xlab="Observed Latitude", ylab="Predicted Latitude")

#predict longitude
y_long_matrix <-as.matrix(y_long)
xy_long_matrix <-data.frame(ind=x_matrix[,c(1:ncol(x_matrix))], dep=y_long_matrix[,c(1)])
xy_long_matrix.lm<-lm(dep~x_matrix,data=xy_long_matrix)

pred_long<-xy_long_matrix.lm$fitted.values
obs_long<-y_long_matrix
#res_long<-xy_long_matrix.lm$residuals
plot(x=obs_long, y=pred_long, type="p", main="Predicted vs. Observed Latitude", xlab="Observed Longitude", ylab="Predicted Longitude")

rsquared_lat <-summary(xy_lat_matrix.lm)$r.squared
rsquared_long <-summary(xy_long_matrix.lm)$r.squared


#Does a Box-Cox transformation improve the regressions? Notice that the dependent variable has some negative values, which Box-Cox doesn't like. You can deal with this by remembering that these are angles, so you get to choose the origin. why do you say so? For the rest of the exercise, use the transformation if it does improve things, otherwise, use the raw data.
pos_ylat <- y_lat_matrix + 90
pos_ylong <- y_long_matrix + 180
boxcox_lat <- boxcox(pos_ylat~x_matrix, lambda = seq(-2, 2, 1/10), plotit = TRUE, xlab = "λ", ylab = "Log Likelihood")
title("BoxCox - Latitude")
boxcox_long <- boxcox(pos_ylong~x_matrix, lambda = seq(-2, 2, 1/10), plotit = TRUE, xlab = "λ", ylab = "Log Likelihood")
title("BoxCox - Longitude")

lambda_lat<-boxcox_lat$x[which.max(boxcox_lat$y)]
lambda_long<-boxcox_long$x[which.max(boxcox_long$y)]
boxcox_y_lat<-(pos_ylat^lambda_lat - 1)/lambda_lat
boxcox_y_long<-(pos_ylong^lambda_long - 1)/lambda_long

boxcox_lat <-data.frame(ind=x_matrix, dep=boxcox_y_lat)
boxcox_lat.lm<-lm(boxcox_y_lat~as.matrix(x_matrix))
boxcox_long <-data.frame(ind=x_matrix, dep=boxcox_y_long)
boxcox_long.lm<-lm(boxcox_y_long~as.matrix(x_matrix))

pred_boxcox_lat<-predict(boxcox_lat.lm,data.frame(x_matrix))
pred_boxcox_long<-predict(boxcox_long.lm,data.frame(x_matrix))

reversed_boxcox_lat<-(pred_boxcox_lat*lambda_lat+1)^(1/lambda_lat)-90
reversed_boxcox_long<-(pred_boxcox_long*lambda_long+1)^(1/lambda_long)-180

rsquared_boxcox_lat <- var(reversed_boxcox_lat)/var(y_lat_matrix)
rsquared_boxcox_long <- var(reversed_boxcox_long)/var(y_lat_matrix)
