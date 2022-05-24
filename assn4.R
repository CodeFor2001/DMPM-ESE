library(dplyr)
library(caret)
library(reshape2)
library(pROC)
library(corrplot)
library(caTools)

flight <- read.csv("FlightDelays.csv")
head(flight)
summary(flight)

 
flight %>% 
  count(delay)

flight <- flight %>%
  mutate(delay = ifelse(delay == "ontime",0,1))

summary(flight)

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

flight[["tailnu"]] <- encode_ordinal(flight[["tailnu"]])
flight[["dest"]] <- encode_ordinal(flight[["dest"]])
flight[["origin"]] <- encode_ordinal(flight[["origin"]])
flight[["carrier"]] <- encode_ordinal(flight[["carrier"]])
head(flight)

flight=within(flight, rm(date))
head(flight)

set.seed(101) 
sample = sample.split(flight$delay, SplitRatio = .60)
train = subset(flight, sample == TRUE)
test  = subset(flight, sample == FALSE)
test_new = within(test, rm(delay))

head(test)
corrplot(cor(train), method="pie",shade.col=NA, tl.col="black", tl.srt=45)



logreg <- glm(delay ~ ., family = binomial(link = 'logit'), data = train)
summary(logreg)

prob <- logreg %>% predict(test_new, type = "response")

test_new$prob = prob

test_new <- test_new %>%
  mutate(predicted = ifelse(prob<0.3,0,1))

head(test_new)

table(test$delay, test_new$predicted)

accuracy = (672+106)/(672+106+37+65)

error_rate = 1- accuracy

precision = 672/(672+37)

recall = 672/(672+65)

cat("Accuracy: ",accuracy*100,"%\nError Rate: ",error_rate*100,"%\nPrecision: ",precision*100,"%\nRecall: ",recall*100,"%")

roc = roc(test$delay ~ prob, plot = TRUE, print.auc = TRUE)

