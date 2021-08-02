set.seed(55)
train <- sample(nrow(winequality), floor(nrow(winequality) * 0.75))
wineTrain <- winequality[train,]
wineTest <- winequality[-train,]
