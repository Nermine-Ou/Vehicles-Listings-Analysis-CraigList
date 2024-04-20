
library(tidyverse)
library(GGally)
library(superml)
library(caret)
library(Boruta)
library(tidymodels)
library(baguette)
library(discrim)
library(bonsai)
library(ggplot2)
library(dplyr)
library(stringr)
library(Hmisc)


install.packages("mltools")

data<-read.table(file=file.choose(),header=T,sep=",",dec=".")
str(data)
summary(data)
dim(data)
names(data)
data <- select(data, -torque)


#missing_values
missing_values <- colSums(is.na(data) | data == "")
missing_values

percentage_of_missing_seats <- sum(is.na(data$seats)) / length(data$seats) * 100
percentage_of_missing_seats 

data$seats <- as.numeric(data$seats)
data$seats[is.na(data$seats)]<-median(data$seats,na.rm=TRUE)

data$mileage <- as.numeric(str_remove(data$mileage, " kmpl"))
data$engine <- as.numeric(str_remove(data$engine, " CC"))
data$max_power <- as.numeric(str_remove(data$max_power, " bhp"))

data$mileage[data$mileage == ""] <- NA
data$engine[data$engine == ""] <- NA
data$max_power[data$max_power == ""] <- NA


str(data)


library(VIM)
data_imp <- kNN(data, variable = c("mileage","engine","max_power"), k = 5)
summary(data_imp)
data$mileage<- data_imp$mileage
data$engine<- data_imp$engine
data$max_power<- data_imp$max_power
str(data)

library(dplyr)
library(ggplot2)

lbl = LabelEncoder$new()
data$fuel<- lbl$fit_transform(data$fuel)
data$name = lbl$fit_transform(data$name)
data$seller_type = lbl$fit_transform(data$seller_type)
data$owner = lbl$fit_transform(data$owner)
data$transmission= lbl$fit_transform(data$transmission)

str(data)
##################################
uppb <- 103.3  + 1.5 *(103.3-68.05)
uppb
lowerb <- 68.05- 1.5 *(273-68.05)
lowerb
outlier_indices <- which(data$max_power> 156.175| data$max_power< -239.375)
outlier_indices
data$max_power[outlier_indices] <- NA
data$max_power<-ifelse (is.na(data$max_power), yes=impute(data$max_power, fun =mean), no=data$max_power)
###################################
uppb <- 1586  + 1.5 *(1586-1197)
uppb
lowerb <- 1197- 1.5 *(1586-1197)
lowerb
outlier_indices <- which(data$engine> 2169.5| data$engine<613.5)
outlier_indices
data$engine[outlier_indices] <- NA
data$engine<-ifelse (is.na(data$engine), yes=impute(data$engine, fun =mean), no=data$engine)
##################################
uppb <- 98000+ 1.5 *(98000-35000)
uppb
lowerb <- 35000- 1.5 *(98000-35000)
lowerb
outlier_indices <- which(data$km_driven > 192500| data$km_driven < -59500)
outlier_indices
data$km_driven [outlier_indices] <- NA
data$km_driven<-ifelse (is.na(data$km_driven), yes=impute(data$km_driven, fun =mean), no=data$km_driven)


str(data)

model1<-lm(selling_price~.,data)
model2<-lm(selling_price~.-owner,data)
summary(model1)
summary(model2)
resid(model1)
AIC(model1)
AIC(model2)

library(corrplot)
corrplot(cor(data), type="full", method ="color", title = "Correlation Plot", mar=c(0,0,1,0), tl.cex= 0.8, outline= T, tl.col="indianred4")
ggcorr(data, label = T)
