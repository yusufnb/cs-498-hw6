library('stats') 
library('MASS')
library('glmnet')

data<-read.csv('default_plus_chromatic_features_1059_tracks.txt', header=FALSE, sep = ',')

names(data)[117]<-"lat"
names(data)[118]<-"lon"

lm.lat = lm(formula = lat ~ .-lon, data)
lm.lon = lm(formula = lon ~ .-lat, data)
plot(lm.lat, which = c(1))
summary(lm.lat)$r.squared
plot(lm.lon, which = c(1))
summary(lm.lon)$r.squared


################# REGULRIZED LINEAR REGRESSION LATITUDE ################# 
#Unregulrized linear regression of latitude against 116 other features
cv.lat.unreg = cv.glmnet(x_matrix,y_lat,alpha = 0, lambda=c(0,0.1), nfolds=10)
plot(cv.lat.unreg)
title("Unregulrized - Latitude")
min(cv.lat.unreg[["lambda"]])
cv.lat.unreg[["cvm"]][which.min(cv.lat.unreg[["lambda"]])]

#Regulrized ridge linear regression of latitude against 116 other features
cv.lat.reg = cv.glmnet(x_matrix,y_lat,alpha = 0, nfolds=10)
plot(cv.lat.reg)
title("Regulrized Ridge - Latitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]


#Regulrized lasso linear regression of latitude against 116 other features
cv.lat.reg = cv.glmnet(x_matrix,y_lat,alpha = 1, nfolds=10)
plot(cv.lat.reg)
title("Regulrized Lasso - Latitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]

#Regulrized elastic net linear regression of latitude with alpha of 0.25
cv.lat.reg = cv.glmnet(x_matrix,y_lat,alpha = .25, nfolds=10)
plot(cv.lat.reg)
title("Regulrized Elastic Net Alpha=0.25 - Latitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]

#Regulrized elastic net linear regression of latitude with alpha of 0.50
cv.lat.reg = cv.glmnet(x_matrix,y_lat,alpha = .50, nfolds=10)
plot(cv.lat.reg)
title("Regulrized Elastic Net Alpha=0.50 - Latitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]

#Regulrized elastic net linear regression of latitude with alpha of 0.75
cv.lat.reg = cv.glmnet(x_matrix,y_lat,alpha = .75, nfolds=10)
plot(cv.lat.reg)
title("Regulrized Elastic Net Alpha=0.75 - Latitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]


################# REGULRIZED LINEAR REGRESSION LONGTITUDE ################# 

#Unregulrized linear regression of longtitude against 116 other features
cv.long.unreg = cv.glmnet(x_matrix,y_long,alpha = 0, lambda=c(0,0.1), nfolds=10)
plot(cv.long.unreg)
title("Unregulrized - Longtitude")
min(cv.long.unreg[["lambda"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]

#Regulrized ridge linear regression of Longtitude against 116 other features
cv.long.reg = cv.glmnet(x_matrix,y_long,alpha = 0, nfolds=10)
plot(cv.long.reg)
title("Regulrized Ridge - Longtitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]

#Regulrized lasso linear regression of Longtitude against 116 other features
cv.long.reg = cv.glmnet(x_matrix,y_long,alpha = 1, nfolds=10)
plot(cv.long.reg)
title("Regulrized Lasso - Longtitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]

#Regulrized elastic net linear regression of Longtitude with alpha of 0.25
cv.long.reg = cv.glmnet(x_matrix,y_long,alpha = .25, nfolds=10)
plot(cv.long.reg)
title("Regulrized Elastic Net Alpha=0.25 - Longtitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]

#Regulrized elastic net linear regression of Longtitude with alpha of 0.50
cv.long.reg = cv.glmnet(x_matrix,y_long,alpha = .50, nfolds=10)
plot(cv.long.reg)
title("Regulrized Elastic Net Alpha=0.50 - Longtitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]

#Regulrized elastic net linear regression of Longtitude with alpha of 0.75
cv.long.reg = cv.glmnet(x_matrix,y_long,alpha = .75, nfolds=10)
plot(cv.long.reg)
title("Regulrized Elastic Net Alpha=0.75 - Longtitude")
min(cv.lat.reg[["cvm"]])
cv.lat.reg[["lambda"]][which.min(cv.lat.reg[["cvm"]])]
cv.lat.reg[["nzero"]][which.min(cv.lat.reg[["cvm"]])]
