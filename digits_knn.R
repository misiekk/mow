library(class)
library(readr)
set.seed(1234)

dataLoaded <- read_csv("~/mow/digits/train.csv")  # 42k rows in general
numNeighbours = 100

nrow(dataLoaded)
dataLoaded$label<-factor(dataLoaded$label)

# threshold values and change them to binary 0 and 1
threshold=20
dataLoaded.pixelVals=dataLoaded[,-1]
dataLoaded.pixelVals[dataLoaded.pixelVals<=threshold] = 0
dataLoaded.pixelVals[dataLoaded.pixelVals>threshold] = 1

#change numeric 0 and 1 to factors
dataLoaded.pixelVals <- data.frame(lapply(dataLoaded.pixelVals, as.factor))
dataLoaded.labels <- dataLoaded$label

#select rows to train and test the model, remove column with variance=0
rowSelector<-sample(2, nrow(dataLoaded), replace=TRUE, prob=c(0.5, 0.5))
colVariance <- apply(dataLoaded.pixelVals, 2, var)
trainPixels <- dataLoaded.pixelVals[rowSelector==1, colVariance>0]
trainLabels <- dataLoaded.labels[rowSelector==1]
testPixels <- dataLoaded.pixelVals[rowSelector==2, colVariance>0]
testLabels <- dataLoaded.labels[rowSelector==2]




#numTrain <- 10000 #10k rows for training-> 32k rows to predict

#rowSelector<-sample(2, nrow(dataLoaded), replace=TRUE, prob=c(0.6, 0.4))
#rowsTrain <- sample(1:nrow(dataLoaded[rowSelector == 1,]), length(rowSelector[rowSelector == 1])) 
#rowsTest <- sample(1:nrow(dataLoaded[rowSelector == 2,]), length(rowSelector[rowSelector == 2])) 

#rowsTrain <- sample(1:nrow(dataLoaded), numTrain) 
#rowsTest <- sample(numTrain:nrow(dataLoaded), nrow(dataLoaded)-numTrain)
#rowsTest <- sample(numTrain:nrow(dataLoaded), nrow(dataLoaded)-numTrain)
#labelsTrain <- as.factor(dataLoaded[rowsTrain,1])
#labelsTest <- as.factor(dataLoaded[rowsTest,1])
#head(labelsTrain)
#head(labelsTest)

# get train and test data from loadedData -> loadedData has labels for each row, needed for testing the algorithm
#trainData <- dataLoaded[rowsTrain,-1]
#testData <- dataLoaded[rowsTest,-1]
#nrow(trainData)
#nrow(testData)

startTime <- Sys.time()
knn <- knn(train = trainPixels, test = testPixels, cl = trainLabels, k = numNeighbours)
endTime <- Sys.time()
duration = endTime - startTime
length(knn)
predictions <- data.frame(ImageId=1:nrow(testPixels), LabelReal=testLabels, LabelPredicted=levels(testLabels)[knn], Result=knn==testLabels)

head(predictions)

errors <- predictions[,4]
table(errors)
fileName <- paste("~/mow/digits/knn_train_", "neigh_", numNeighbours, ".csv", sep="")

write_csv(predictions, fileName) 
duration
