library(tree)
library(rpart)
library(rpart.plot)
library(vip)
library(Metrics)
library(magrittr)
library(rsample)
prostate = read.csv("D:/TY sem6/DMPM LAB/Assn6/prostate.csv")

head(prostate)
str(prostate)
dim(prostate) #97x6
summary(prostate)
#no NANs, need to scale the data
#lcavol - response variable


#split
set.seed(123) 
sample_ind = sample(nrow(prostate),nrow(prostate)*0.80)
train = prostate[sample_ind,]
test = prostate[-sample_ind,]

dim(train)
dim(test)


pstree <- rpart(
  formula = lcavol ~ .,
  data    = train,
  method  = "anova"
  , control = list(cp = 0, maxdepth = 30,minsplit = 20)
)
rpart.plot(pstree)
plotcp(pstree)
preds = predict(pstree, test)

cat("RMSE: ", rmse(test$lcavol,preds),"\nMAE: ", mae(test$lcavol,preds),
    "\nMSE: ", mse(test$lcavol,preds))

rpart.plot(pstree)
plotcp(pstree)

printcp(pstree)

vip(pstree, num_features = 5)

#pruning 

prunedTree <- rpart(
  formula = lcavol ~ .,
  data    = train,
  method  = "anova"
  , control = list(cp = 0.01)
)
preds2 = predict(prunedTree, test)

cat("RMSE: ", rmse(test$lcavol,preds2),"\nMAE: ", mae(test$lcavol,preds2),
    "\nMSE: ", mse(test$lcavol,preds2))
rpart.plot(prunedTree)
plotcp(prunedTree)


prunedTree2 <- rpart(
  formula = lcavol ~ .,
  data    = train,
  method  = "anova"
  , control = list(cp = 0.01,xval=10)
  
)
preds3 = predict(prunedTree2, test)
cat("RMSE: ", rmse(test$lcavol,preds3),"\nMAE: ", mae(test$lcavol,preds3),
    "\nMSE: ", mse(test$lcavol,preds3))
rpart.plot(prunedTree2)

