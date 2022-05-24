library(dplyr)
library(tidyverse)
library(janitor)
library(Hmisc)
library(caret)
library(reshape2)
library(caTools)
library(ggplot2)
library(scales)
library(Metrics)
lifedf <- read.csv("LifeExpectancyData.csv")
head(lifedf)
summary(lifedf)
glimpse(lifedf)
names(lifedf)

lifedf=clean_names(lifedf)
names(lifedf)

dim(lifedf)

na_count <-sapply(lifedf, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count


lifedf1 = lifedf %>%
  filter(!is.na(life_expectancy),
         !is.na(adult_mortality),
         !is.na(hepatitis_b),
         !is.na(bmi),
         !is.na(polio),
         !is.na(diphtheria),
         !is.na(hiv_aids),
         !is.na(total_expenditure),
         !is.na(thinness_1_19_years),
         !is.na(thinness_5_9_years),
         !is.na(alcohol),
         !is.na(income_composition_of_resources),
         !is.na(schooling),
         )
summary(lifedf1)
dim(lifedf1)

lifedf1$population = impute(lifedf1$population, fun = median) # median imputation
lifedf1$gdp = impute(lifedf1$gdp, fun = median) # median imputation


na_count <-sapply(lifedf1, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

process <- preProcess(as.data.frame(lifedf1), method=c("range"))

lifedf1 = predict(process, as.data.frame(lifedf1))

lifedf1$year

data<-cor(lifedf1[sapply(lifedf1, is.numeric)])
data=melt(data)
correlation = subset(data,data$Var2=="life_expectancy")
correlation


ggplot(data,aes(x = Var1, y = Var2,fill = value))+
  geom_tile()+
  theme(axis.text.x=element_text(angle=90))

#map 
library(maps)
mapdata<-map_data("world")
glimpse(mapdata)
mapdata=left_join(lifedf1,mapdata,by="region")
glimpse(mapdata)

map1 = ggplot(mapdata,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=life_expectancy), color="black")
map1

map2 = map1+ scale_fill_gradient(name="LifeExpectancy",low="yellow",high="red", na.value="grey50")+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        )

map2

plot(x= lifedf1$schooling, y=lifedf1$life_expectancy, xlab="Schooling", ylab="LifeExpectancy",
     main="Schooling vs Life Expectancy")
plot(x= lifedf1$hiv_aids, y=lifedf1$life_expectancy, xlab="hiv_aids", ylab="LifeExpectancy",
     main="hiv_aids vs Life Expectancy")
plot(x= lifedf1$total_expenditure, y=lifedf1$life_expectancy, xlab="Expenditure", ylab="LifeExpectancy",
     main="total_expenditure vs Life Expectancy")
plot(x= lifedf1$bmi, y=lifedf1$life_expectancy, xlab="BMI", ylab="LifeExpectancy",
     main="BMI vs Life Expectancy")
plot(x= lifedf1$income_composition_of_resources, y=lifedf1$life_expectancy, xlab="income_composition_of_resources", ylab="LifeExpectancy",
     main="income_composition_of_resources vs Life Expectancy")
plot(x= lifedf1$gdp, y=lifedf1$life_expectancy, xlab="GDP", ylab="LifeExpectancy",
     main="GDP vs Life Expectancy")
plot(x= lifedf1$adult_mortality, y=lifedf1$life_expectancy, xlab="adult_mortality", ylab="LifeExpectancy",
     main="Adult Mortality vs Life Expectancy")


#split
set.seed(101) 
sample = sample.split(lifedf1$life_expectancy, SplitRatio = .70)
train = subset(lifedf1, sample == TRUE)
test  = subset(lifedf1, sample == FALSE)
test_new = within(test, rm(life_expectancy))

#linear regression model
#country+year+infant_deaths+under_five_deaths+hiv_aids+thinness_5_9_years
model1 = lm(life_expectancy~ region+schooling+income_composition_of_resources+hiv_aids , data = train )
summary(model1)

prob = model1 %>% predict(test_new)

test_new$predictedExpectancy = prob

x=cbind(test$life_expectancy,prob)
x=data.matrix(x) 
x=rescale(x)
x=as.data.frame(x)
mae=mae(x$V1,x$prob) 
mse=mse(x$V1,x$prob)
rmse=rmse(x$V1,x$prob)
cat("\nMAE:",mae,"\n\nMSE:",mse,"\n\nRMSE:",rmse,"\n\n") 

resid = resid(model1)

plot(train$life_expectancy,resid,
     main = "Residual Plot(Schooling and life expectancy)",
     abline(0,0), ylab = "Residuals", xlab
     = "Age(in years)")
