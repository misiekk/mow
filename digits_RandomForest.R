library(randomForest)
library(readr)
set.seed(1234)

dataLoaded <- read_csv("~/mow/digits/train.csv")  # 42k rows in general
nrow(dataLoaded)
dataLoaded$label<-factor(dataLoaded$label)
numTrees <- 15

# threshold values and change them to binary 0 and 1
threshold=20
dataLoaded.pixelVals=dataLoaded[,-1]
dataLoaded.pixelVals[dataLoaded.pixelVals<=threshold] = 0
dataLoaded.pixelVals[dataLoaded.pixelVals>threshold] = 1

#change numeric 0 and 1 to factors
dataLoaded.pixelVals <- data.frame(lapply(dataLoaded.pixelVals, as.factor))
dataLoaded.labels <- dataLoaded$label

#select rows to train and test the model, remove column with variance=0
rowSelector<-sample(2, nrow(dataLoaded), replace=TRUE, prob=c(0.7, 0.3))
colVariance <- apply(dataLoaded.pixelVals, 2, var)
trainPixels <- dataLoaded.pixelVals[rowSelector==1, colVariance>0]
trainLabels <- dataLoaded.labels[rowSelector==1]
testPixels <- dataLoaded.pixelVals[rowSelector==2, colVariance>0]
testLabels <- dataLoaded.labels[rowSelector==2]

startTime <- Sys.time()
rf <- randomForest(trainPixels, trainLabels, xtest=testPixels, ntree=numTrees)
endTime <- Sys.time()
duration = endTime - startTime

predictions <- data.frame(ImageId=1:nrow(testPixels), LabelReal=testLabels, LabelPredicted=levels(testLabels)[rf$test$predicted], Result=rf$test$predicted==testLabels)

errors <- predictions[,4]
table(errors)
fileName <- paste("~/mow/digits/randomforest", numTrees, ".csv", sep="")

write_csv(predictions, fileName) 
duration
