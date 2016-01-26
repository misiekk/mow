library(randomForest)
library(plyr)
library(class)
library(klaR)
library(readr)
set.seed(1234)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
trainLabels <- iris$Species[ind==1]
testLabels <- iris$Species[ind==2]
trainData <- trainData[, -5]
testData <- testData[, -5]
numTrees = 20
numNeighbours = 13


##### NAIVE BAYES
model <- NaiveBayes(trainData,trainLabels, fL=1)
startTime <- Sys.time()
predictions <- predict(model, testData)
endTime <- Sys.time()
duration = endTime - startTime
print("NaiveBayes:")
duration
toWrite <- data.frame(Rownum=names(predictions$class), Label=predictions$class, RealLabel=testLabels)
toWrite<-transform(toWrite, Result=predictions$class==testLabels)
write_csv(toWrite, "~/mow/iris/resultsNaiveBayes.csv")
errorsNB<-toWrite[,4]
table(errorsNB)



##### RANDOM FOREST
startTime <- Sys.time()
rf <- randomForest(trainData, trainLabels, xtest=testData, ntree=numTrees)
endTime <- Sys.time()
duration = endTime - startTime
predictions <- data.frame(ImageId=1:nrow(testData), LabelReal=testLabels, LabelPredicted=levels(testLabels)[rf$test$predicted], Result=rf$test$predicted==testLabels)
errors <- predictions[,4]
table(errors)
fileName <- paste("~/mow/iris/randomforest", numTrees, ".csv", sep="")
write_csv(predictions, fileName) 
print("RF:")
duration



##### K-NEAREST NEIGHBOURS
# prepare data 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

iris_new <- as.data.frame(lapply(iris[, c(1,2,3,4)], normalize))

startTime <- Sys.time()
knn <- knn(train = trainData, test = testData, cl = trainLabels, k = numNeighbours)
endTime <- Sys.time()
duration = endTime - startTime
length(knn)
predictions <- data.frame(ImageId=1:nrow(testData), LabelReal=testLabels, LabelPredicted=levels(testLabels)[knn], Result=knn==testLabels)
errors <- predictions[,4]
table(errors)
fileName <- paste("~/mow/iris/knn_train_", "neigh_", numNeighbours, ".csv", sep="")
write_csv(predictions, fileName) 
print("KNN:")
duration
