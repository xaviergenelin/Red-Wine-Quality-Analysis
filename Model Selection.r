# Model Selection

### Forward Selection
library(leaps)
forward <- regsubsets(quality~., data=wineTrain, nvmax=11, method="forward")
summary(forward)
forwardTest <- model.matrix(quality~., data=wineTest, nvmax=11)
forwardTestMSE <- rep(NA,11)
for(i in 1:11){
  forwardCoef <- coef(forward, id=i)
  forwardPred <- forwardTest[,names(forwardCoef)] %*% forwardCoef
  forwardTestMSE[i] <- mean((wineTest$quality - forwardPred)^2)
  }
forwardMin <- which.min(forwardTestMSE)
forwardTestMSE[forwardMin]
coef(forward, id=forwardMin)

### Backward Selection
library(leaps)
backward <- regsubsets(quality~., data=wineTrain, nvmax=11, method="forward")
summary(backward)
backwardTest <- model.matrix(quality~., data=wineTest, nvmax=11)
backwardTestMSE <- rep(NA,11)
for(i in 1:11){
  backwardCoef <- coef(backward, id=i)
  backwardPred <- backwardTest[,names(backwardCoef)] %*% backwardCoef
  backwardTestMSE[i] <- mean((wineTest$quality - backwardPred)^2)
  }
backwardMin <- which.min(backwardTestMSE)
backwardTestMSE[backwardMin]
coef(backward, id=backwardMin)

### Best Subset Selection
library(leaps)
bestSub <- regsubsets(quality~., data=wineTrain, nvmax=11)
summary(bestSub)
bestSubTest <- model.matrix(quality~., data=wineTest, nvmax=11)
bestSubTestMSE <- rep(NA,11)
for(i in 1:11){
  bestSubCoef <- coef(bestSub, id=i)
  bestSubPred <- bestSubTest[,names(bestSubCoef)] %*% bestSubCoef
  bestSubTestMSE[i] <- mean((wineTest$quality - bestSubPred)^2)
}
bestSubMin <- which.min(bestSubTestMSE)
bestSubTestMSE[bestSubMin]
coef(bestSub, id=7)