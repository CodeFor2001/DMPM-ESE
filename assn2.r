library(dplyr)
library(scales)
carsdf <- read.csv("ToyotaCorolla.csv")
head(carsdf)
summary(carsdf)
summary(carsdf$FuelType)
str(carsdf)

carsdf %>% 
  count(FuelType)

#lets encode the FuelType, 1=CNG, 2= Deisel, 3= petrol
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

carsdf[["Fuel_encoded"]] <- encode_ordinal(carsdf[["FuelType"]])
head(carsdf)

carsdf %>% 
  count(Fuel_encoded)

model <- lm(Price~Age+KM+HP+MetColor+Automatic+CC+Doors+Weight+Fuel_encoded, data=carsdf)

summary(model)

print(model)

pred<-predict(model)
resi <- resid(model)

#lets prove the correlations by plotting the scatter plot

plot(carsdf$Price,carsdf$Age,
     main="Age and Price",
     abline(lm(carsdf$Age~carsdf$Price)),
     ylab = "Age(in Years)",
     xlab="Price(in EUROS)")

plot(carsdf$Price,carsdf$KM,
     main="KM and Price",
     abline(lm(carsdf$KM~carsdf$Price)), 
     ylab = "Kilometers(KM)",
     xlab="Price(in EUROS)") 

plot(carsdf$Price,carsdf$HP,
     main="HorsePower(HP) and Price",
     abline(lm(carsdf$HP~carsdf$Price)), ylab =
       "HorsePower(HP)", xlab="Price(in
EUROS)") 

plot(carsdf$Price,carsdf$CC,
     main="Cylinder volume(in cc) and Price",
     abline(lm(carsdf$CC~carsdf$Price)), ylab =
       "Cylinder Volume(in cc)", xlab="Price(in
EUROS)") 

plot(carsdf$Price,carsdf$Weight,
     main="Weight and Price",
     abline(lm(carsdf$Weight~carsdf$Price)), ylab
     = "Weight(in kg)", xlab="Price(in
EUROS)") 

plot(carsdf$Age,resi,
     main = "Residual Plot(Age and Price)",
     abline(0,0), ylab = "Residuals", xlab
     = "Age(in years)")
plot(carsdf$KM,resi,
     main = "Residual Plot(KM and Price)",
     abline(0,0), ylab = "Residuals",
     xlab = "KM") 
plot(carsdf$HP,resi,
     main = "Residual Plot(HP and Price)",
     abline(0,0), ylab = "Residuals", xlab =
       "HP") 
plot(carsdf$CC,resi,
     main = "Residual Plot(CC and Price)",
     abline(0,0), ylab = "Residuals", xlab =
       "CC")
plot(carsdf$Weight,resi,
     main = "Residual Plot(Weight and Price)",
     abline(0,0), ylab = "Residuals", xlab =
       "Weight(in kg)") 



#evaluation
x<-cbind(carsdf$Price,pred)
x<-data.matrix(x) 
x<-rescale(x)
x<-as.data.frame(x)
mae<-mae(x$V1,x$pred) 
mse<-mse(x$V1,x$pred)
rmse<-rmse(x$V1,x$pred)
cat("\nMAE:",mae,"\n\nMSE:",mse,"\n\nRMSE:",rmse,"\n\n") 
