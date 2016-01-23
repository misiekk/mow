library(class)
library(readr)
set.seed(0)

dataLoaded <- read_csv("~/mow/digits/train.csv")  # 42k rows in general
nrow(dataLoaded)

numTrain <- 10000 #10k rows for training-> 32k rows to predict
numNeighbours <- 150

rowsTrain <- sample(1:nrow(dataLoaded), numTrain) 
#rowsTest <- sample(numTrain:nrow(dataLoaded), nrow(dataLoaded)-numTrain)
rowsTest <- sample(numTrain:nrow(dataLoaded), nrow(dataLoaded)-numTrain)
labelsTrain <- as.factor(dataLoaded[rowsTrain,1])
labelsTest <- as.factor(dataLoaded[rowsTest,1])
#head(labelsTrain)
#head(labelsTest)

# get train and test data from loadedData -> loadedData has labels for each row, needed for testing the algorithm
trainData <- dataLoaded[rowsTrain,-1]
testData <- dataLoaded[rowsTest,-1]
nrow(trainData)
nrow(testData)

startTime <- Sys.time()
knn <- knn(train = trainData, test = testData, cl = labelsTrain, k = numNeighbours)
endTime <- Sys.time()
duration = endTime - startTime
length(knn)
predictions <- data.frame(ImageId=1:nrow(testData), LabelReal=labelsTest, LabelPredicted=levels(labelsTest)[knn], Result=knn==labelsTest)

head(predictions)

errors <- predictions[,4]
table(errors)
fileName <- paste("~/mow/digits/knn_train", numTrain, "neigh_", numNeighbours, ".csv", sep="")

write_csv(predictions, fileName) 
duration
