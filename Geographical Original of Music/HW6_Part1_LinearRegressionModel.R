setwd("~/Dev/CS498/HW6/cs-498-hw6/Geographical Original of Music")
library('stats') 

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