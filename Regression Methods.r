## OLS

# Interaction

# Polynomials

# Lasso
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

# Ridge
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


# Decision Tree
 
# General Additive Models