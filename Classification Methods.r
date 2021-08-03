# Change quality variable to 1 or 0
wineTrain$quality <- ifelse(wineTrain$quality >= 7, 1, 0)

wineTest$quality <- ifelse(wineTest$quality >= 7, 1, 0)

### Logistic
logModel <- glm(quality ~ ., data = wineTrain, family = "binomial")
logPred <- predict(logModel, newdata = wineTest, type = "response")
logPred <- ifelse(logPred >= 0.5, 1, 0)
table(logPred, wineTest$quality)

# Test error rate
mean(logPred != wineTest$quality)

### Linear Discriminant Analysis
library(MASS)
ldaModel <- lda(quality ~ ., data = wineTrain)
ldaPred <- predict(ldaModel, newdata = wineTest, type = "response")
table(ldaPred$class, wineTest$quality)

# Test error rate
mean(ldaPred$class != wineTest$quality)

### Quadratic Discriminant Analysis
qdaModel <- qda(quality ~ ., data = wineTrain)
qdaPred <- predict(qdaModel, newdata = wineTest)$class
table(qdaPred, wineTest$quality)

# Test error rate
mean(qdaPred != wineTest$quality) 

### Decision Tree 
library(tree)
wineTrain <- winequality[train, ]
wineTest <- winequality[-train, ]
wineTrain$quality <- factor(ifelse(wineTrain$quality >= 7, "High", "Low"))
wineTest$quality <- factor(ifelse(wineTest$quality >= 7, "High", "Low"))
wineTrain <- data.frame(wineTrain)
wineTest <- data.frame(wineTest)
treeModel <- tree(quality ~ ., data = wineTrain)
# Plot out the tree
plot(treeModel)
text(treeModel, pretty = 0, cex = 0.7)

treePred <- predict(treeModel, wineTest, type = "class")
table(treePred, wineTest$quality)
# Test error rate
mean(treePred != wineTest$quality)

# Use cross-validation to prune the tree
set.seed(33)
cvTree <- cv.tree(treeModel, FUN = prune.misclass)
plot(cvTree$size, cvTree$dev, type = "b")
pruneTree <- prune.misclass(treeModel, best = 5)
plot(pruneTree)
text(pruneTree, pretty = 0)
prunePred <- predict(pruneTree, wineTest, type = "class")

table(prunePred, wineTest$quality)

# Test error rate
mean(prunePred != wineTest$quality)

### Random Forest
library(randomForest)
set.seed(5)
rfModel <- randomForest(quality ~ ., data = wineTrain, importance = TRUE, ntree = 25)
rfPred <- predict(rfModel, newdata = wineTest)
wineQuality <- ifelse(wineTest$quality == "High", 1, 0)
rfQuality <- ifelse(rfPred == "High", 1, 0)
table(rfQuality, wineQuality)

# Test error rate
(9+26)/400

### K-Nearest Neighbors
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
# Test error rate
mean(knnPred != yTest)

### Support Vector Machine
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

# Test error rate
(1+31)/400