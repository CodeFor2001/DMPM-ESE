library(dplyr)
library(tidyverse)	
library(dplyr)	
library(ggplot2)	
library(Hmisc)
dirtydf <- read.csv("dirty.csv")

head(dirtydf)
summary(dirtydf)
glimpse(dirtydf)

na_count <-sapply(dirtydf, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count


par( mfrow= c(1,3) )
boxplot(dirtydf$Age,main="Age")
boxplot(dirtydf$Weight,main="Weight")
boxplot(dirtydf$Price,main="Price")

outlier_values <- boxplot.stats(dirtydf$Price)$out  # outlier values.
print(outlier_values)

omitdf<-na.omit(dirtydf)
cat("Percentage of missing values in the na omitted dataset",mean(is.na(omitdf)),"%")

#imputing age with median values and weight w/ mean values
dirtydf$Weight = impute(dirtydf$Weight, fun = mean) # mean imputation
dirtydf$Age = impute(dirtydf$Age, fun = median) # median imputation

sum(is.na(dirtydf$Weight))
sum(is.na(dirtydf$Age))

dirtydf=na.omit(dirtydf$CC)
sum(is.na(dirtydf))

