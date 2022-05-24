data<-read.csv("HT-WT-Age.csv")
head(data)

#First model 
model1 <- lm(data$Weight~data$Height)
summary(model1)

residual1<-resid(model1)

#residual plot
plot(data$Height, residual1, ylab="Residuals", xlab="Height",
     main = "Residual Plot of model1") 
abline(0, 0)     

#scatter plot
plot(data$Height,data$Weight, ylab="Height", xlab="weight",
     main = "Scatterplot between Height and Weight")


#Second model
model2 <- lm(data$Weight~data$Age)
summary(model2)

residual2<-resid(model2)

#residual plot
plot(data$Age, residual2, ylab="Residuals", xlab="Weight",
     main = "Residual Plot of model2")
abline(0, 0)     
#scatterplot
plot(data$Age, data$Weight, ylab="Age", xlab="Weight",
     main = "Scatterplot between Age and Weight")


