# Classification

# Logistic
logModel <- glm(quality ~ ., data = wineTrain, family = "binomial")
logPred <- predict(logModel, newdata = wineTest, type = "response")
logPred <- ifelse(logPred >= 0.5, 1, 0)
table(logPred, wineTest$quality)

# Linear Discriminant Analysis
library(MASS)
ldaModel <- lda(quality ~ ., data = wineTrain)
ldaPred <- predict(ldaModel, newdata = wineTest, type = "response")
table(ldaPred$class, wineTest$quality)

# Quadratic Discriminant Analysis
qdaModel <- qda(quality ~ ., data = wineTrain)
qdaPred <- predict(qdaModel, newdata = wineTest)$class
table(qdaPred, wineTest$quality)

# Decision Tree 
library(tree)
wineTrain <- winequality[train, ]
wineTest <- winequality[-train, ]
wineTrain$quality <- factor(ifelse(wineTrain$quality >= 7, "High", "Low"))
wineTest$quality <- factor(ifelse(wineTest$quality >= 7, "High", "Low"))
wineTrain <- data.frame(wineTrain)
wineTest <- data.frame(wineTest)
treeModel <- tree(quality ~ ., data = wineTrain)
plot(treeModel)
text(treeModel, pretty = 0, cex = 0.7)
treePred <- predict(treeModel, wineTest, type = "class")
table(treePred, wineTest$quality)
(15+342)/400 = 0.8925 prediction accuracy
set.seed(33)
cvTree <- cv.tree(treeModel, FUN = prune.misclass)
plot(cvTree$size, cvTree$dev, type = "b")
pruneTree <- prune.misclass(treeModel, best = 5)
plot(pruneTree)
text(pruneTree, pretty = 0)
prunePred <- predict(pruneTree, wineTest, type = "class")

table(prunePred, wineTest$quality)

# Random Forest
library(randomForest)
set.seed(5)
rfModel <- randomForest(quality ~ ., data = wineTrain, importance = TRUE, ntree = 25)
rfPred <- predict(rfModel, newdata = wineTest)
wineQuality <- ifelse(wineTest$quality == "High", 1, 0)
rfQuality <- ifelse(rfPred == "High", 1, 0)
table(rfQuality, wineQuality)

# K-Nearest Neighbors
xTrain <- wineTrain[, -12]
yTrain <- wineTrain[, 12, drop = TRUE]
xTest <- wineTest[, -12]
yTest <- wineTest[, 12, drop = TRUE]
error <- as.numeric()
set.seed(33)
for(i in 1:10){
knnPred <- knn(xTrain, xTest, yTrain, k = i)
error[i] <- mean(knnPred != yTest)
}
23
plot(error)

# Support Vector Machine
xTrain <- wineTrain[, -12]
yTrain <- wineTrain[, 12, drop = TRUE]
xTest <- wineTest[, -12]
yTest <- wineTest[, 12, drop = TRUE]
error <- as.numeric()
set.seed(33)
for(i in 1:10){
knnPred <- knn(xTrain, xTest, yTrain, k = i)
error[i] <- mean(knnPred != yTest)
}

plot(error)
