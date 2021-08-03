### OLS
ols <- glm(quality~., data=wineTrain)
summary(ols)
olsPred <- predict(ols, newdata=wineTest)
# Test MSE
mean((wineTest$quality-olsPred)^2)
plot(ols)

### Interaction
interactions <- lm(quality~.^2, data=wineTrain)
summary(interactions)
plot(interactions)

interactPred <- predict(interactions, newdata=wineTest)
# Test MSE
mean((wineTest$quality-interactPred)^2)

### Polynomials
quad <- lm(quality~.+I(fixed.acidity^2)+I(volatile.acidity^2)+I(citric.acid^2)+I(residual.sugar^2)+I(chlor17ides^2)+I(free.sulfur.dioxide^2)+I(total.sulfur.dioxide^2)+I(density^2)+I(pH^2)+I(sulphates^2)+I(alcohol^2), data=wineTrain)
summary(quad)
cubic <- lm(quality~.+I(sulphates^2)+I(sulphates^3), data=wineTrain)
summary(cubic)
quar <- lm(quality~.+I(sulphates^2)+I(sulphates^3)+I(sulphates^4), data=wineTrain)
summary(quar)
cubicPred <- predict(cubic, newdata=wineTest)

# Test MSE
mean((wineTest$quality-cubicPred)^2)

### Lasso
library(glmnet)
grid<-10^seq(10,-2,length=100)

xTrain <- model.matrix(quality ~ ., data = wineTrain)[, -1]
yTrain <- wineTrain$quality

lasso <- glmnet(xTrain, yTrain, alpha = 1, lambda = grid) 

set.seed(13)
cvLasso <- cv.glmnet(xTrain, yTrain, alpha = 1, nfolds = 11)

bestLambda <- cvLasso$lambda.min

bestLambda

xTest <- model.matrix(quality ~ ., data = wineTest)[, -1]
lassoPred <- predict(lasso, s = bestLambda, newx = xTest)

mean((lassoPred - wineTest$quality)^2)

lassoCoef <- predict(lasso, type = "coefficients", s = bestLambda)
lassoCoef

### Ridge
grid <- 10^seq(10, -2, length = 100)

xTrain <- model.matrix(quality ~ ., data = wineTrain)[, -1]
yTrain <- wineTrain$quality

ridge <- glmnet(xTrain, yTrain, alpha = 0, lambda = grid)

set.seed(13)

cvRidge <- cv.glmnet(xTrain, yTrain, alpha = 0, nfolds = 11)

bestLambda <- cvRidge$lambda.min
bestLambda

ridgePred <- predict(ridge, s = bestLambda, newx = model.matrix(quality ~ ., data = wineTest)[, -1])

yTest <- wineTest$quality

mean((yTest - ridgePred)^2)


### Decision Tree
library(tree)
treeReg <- tree(quality~., data=wineTrain)
summary(treeReg)
plot(treeReg)
text(treeReg, pretty=0)
treePred <- predict(treeReg, newdata=wineTest)
mean((treePred - wineTest$quality)^2)

set.seed(21)
treeCV <- cv.tree(treeReg)
treeCV
plot(treeCV$size, treeCV$dev, type="b", main="Regression Tree Cross Validation Error")
pruneTree<- prune.tree(wine.tree, best=9)
plot(pruneTree)
text(pruneTree, pretty=0)
prunePred <- predict(pruneTree, newdata=wineTest)
mean((prunePred - wineTest$quality)^2)

pruneTree <- prune.tree(treeReg, best=6)
plot(pruneTree)
text(pruneTree, pretty=0)
prunePred <- predict(pruneTree, newdata=wineTest)
mean((prunePred-wineTest$quality)^2)
 
### General Additive Models
library(splines)
library(gam)
gamTwoDF <- lm(quality~ns(fixed.acidity,2)+ns(volatile.acidity,2)+ns(citric.acid,2)+ns(residual.sugar,2)+ns(chlorides,2)+ns(free.sulfur.dioxide,2)+ns(total.sulfur.dioxide,2)+ns(density,2)+ns(pH,2)+ns(sulphates,2)+ns(alcohol,2), data=wineTrain)
gamDF2Pred <- predict(gamTwoDF, newdata=wineTest)
mean((wineTest$quality-gamDF2Pred)^2)

gamThreeDF <- lm(quality~ns(fixed.acidity,3)+ns(volatile.acidity,3)+ns(citric.acid,3)+ns(residual.sugar,3)+ns(chlorides,3)+ns(free.sulfur.dioxide,3)+ns(total.sulfur.dioxide,3)+ns(density,3)+ns(pH,3)+ns(sulphates,3)+ns(alcohol,3), data=wineTrain)
gamDF3Pred <- predict(gamThreeDF, newdata=wineTest)
mean((wineTest$quality-gamDF3Pred)^2)

gamFourDF <- lm(quality~ns(fixed.acidity,4)+ns(volatile.acidity,4)+ns(citric.acid,4)+ns(residual.sugar,4)+ns(chlorides,4)+ns(free.sulfur.dioxide,4)+ns(total.sulfur.dioxide,4)+ns(density,4)+ns(pH,4)+ns(sulphates,4)+ns(alcohol,4), data=wineTrain)
gamDF4Pred <- predict(gamFourDF, newdata=wineTest)
mean((wineTest$quality-gamDF4Pred)^2)

gamFiveDF <- lm(quality~ns(fixed.acidity,5)+ns(volatile.acidity,5)+ns(citric.acid,5)+ns(residual.sugar,5)+ns(chlorides,5)+ns(free.sulfur.dioxide,5)+ns(total.sulfur.dioxide,5)+ns(density,5)+ns(pH,5)+ns(sulphates,5)+ns(alcohol,5), data=wineTrain)
gamDF5Pred <- predict(gamFiveDF, newdata=wineTest)
mean((wineTest$quality-gamDF5Pred)^2)

gamTwoDFSmooth <- gam(quality~s(fixed.acidity,2)+s(volatile.acidity,2)+s(citric.acid,2)+s(residual.sugar,2)+s(chlorides,2)+s(free.sulfur.dioxide,2)+s(total.sulfur.dioxide,2)+s(density,2)+s(pH,2)+s(sulphates,2)+s(alcohol,2), data=wineTrain)
gamDF2PredSmooth <- predict(gamTwoDFSmooth, newdata=wineTest)
mean((wineTest$quality-gamDF2PredSmooth)^2)

gamThreeDFSmooth <- gam(quality~s(fixed.acidity,3)+s(volatile.acidity,3)+s(citric.acid,3)+s(residual.sugar,3)+s(chlorides,3)+s(free.sulfur.dioxide,3)+s(total.sulfur.dioxide,3)+s(density,3)+s(pH,3)+s(sulphates,3)+s(alcohol,3), data=wineTrain)
gamDF3PredSmooth <- predict(gamThreeDFSmooth, newdata=wineTest)
mean((wineTest$quality-gamDF3PredSmooth)^2)

gamFourDFSmooth <- gam(quality~s(fixed.acidity,4)+s(volatile.acidity,4)+s(citric.acid,4)+s(residual.sugar,4)+s(chlorides,4)+s(free.sulfur.dioxide,4)+s(total.sulfur.dioxide,4)+s(density,4)+s(pH,4)+s(sulphates,4)+s(alcohol,4), data=wineTrain)
gamDF4PredSmooth <- predict(gamFourDFSmooth, newdata=wineTest)
mean((wineTest$quality-gamDF4PredSmooth)^2)

gamFiveDFSmooth <- gam(quality~s(fixed.acidity,5)+s(volatile.acidity,5)+s(citric.acid,5)+s(residual.sugar,5)+s(chlorides,5)+s(free.sulfur.dioxide,5)+s(total.sulfur.dioxide,5)+s(density,5)+s(pH,5)+s(sulphates,5)+s(alcohol,5), data=wineTrain)
gamDF5PredSmooth <- predict(gamFiveDFSmooth, newdata=wineTest)
mean((wineTest$quality-gamDF5PredSmooth)^2)
