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
