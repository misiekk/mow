library(plyr)
library(class)
library(readr)
library(randomForest)
set.seed(1234)
#dataLoaded <- read.csv("~/mow/crime/train.csv", na.strings=c("", "NA", "NULL"))
dataLoaded <- read_csv("~/mow/crime/train.csv")  # 42k rows in general

nrow(dataLoaded)
dataLoaded <- dataLoaded[,-(1)]
dataLoaded <- dataLoaded[,-(6)]
#dataLoaded <- dataLoaded[,-(3:6)]

desc <- unique(dataLoaded$Descript)
dayofweek <- unique(dataLoaded$DayOfWeek)
pdDistr <- unique(dataLoaded$PdDistrict)
resol <- unique(dataLoaded$Resolution)
address <- unique(dataLoaded$Address) # 23k unique adresses, let's forget about it

descriptionMap <- data.frame(id=1:length(desc), descr=desc) #879
dayofweekMap <- data.frame(id=1:length(dayofweek), descr=dayofweek) # 7
pdDistrMap <- data.frame(id=1:length(pdDistr), descr=pdDistr) #10
resolMap <- data.frame(id=1:length(resol), descr=resol) #17



dataLoaded$Descript <- mapvalues(dataLoaded$Descript, from = descriptionMap$descr, to = descriptionMap$id)
dataLoaded$DayOfWeek <- mapvalues(dataLoaded$DayOfWeek, from = dayofweekMap$descr, to = dayofweekMap$id)
dataLoaded$PdDistrict <- mapvalues(dataLoaded$PdDistrict, from = pdDistrMap$descr, to = pdDistrMap$id)
dataLoaded$Resolution <- mapvalues(dataLoaded$Resolution, from = resolMap$descr, to = resolMap$id)


#dataLoaded$Dates <- as.Date(dataLoaded$Dates)

#dataLoaded <- dataLoaded[1:200,]
head(dataLoaded)


#dataLoaded$Descript[1] <- as.integer(dataLoaded$Descript[1])
#for (i in 1:nrow(dataLoaded)){
# tmp <- dataLoaded$Descript[i]
# for(j in 1:length(desc)){
#   if (desc[j] == tmp){
#     dataLoaded$Descript[i] <- as.integer(dataLoaded$Descript[i])
#     dataLoaded$Descript[i] = j
#     break
#   }
# }
#}
#length(desc)
#length(unique(dataLoaded$Descript))


#dataLoaded$Descript <- as.factor(dataLoaded$Descript)
#dataLoaded$Descript[] <- data.frame(lapply(dataLoaded$Descript, as.factor))
#dataLoaded <- dataLoaded[,-7]
numTrain <- 600000 #10k rows for training-> 32k rows to predict
numTrees <- 25


rowSelector<-sample(2, nrow(dataLoaded), replace=TRUE, prob=c(0.6, 0.4))
rowsTrain <- sample(1:nrow(dataLoaded[rowSelector == 1,]), length(rowSelector[rowSelector == 1])) 
rowsTest <- sample(1:nrow(dataLoaded[rowSelector == 2,]), length(rowSelector[rowSelector == 2])) 
#rowsTrain <- sample(1:nrow(dataLoaded), numTrain) 
#rowsTest <- sample(numTrain:nrow(dataLoaded), nrow(dataLoaded)-numTrain)

length(rowsTrain)
length(rowsTest)
labelsTrain <- as.factor(dataLoaded[rowsTrain,1])
labelsTest <- as.factor(dataLoaded[rowsTest,1])
head(labelsTrain)
head(labelsTest)
trainData <- dataLoaded[rowsTrain,-1]
testData <- dataLoaded[rowsTest,-1]
head(trainData)
head(testData)
startTime <- Sys.time()
rf <- randomForest(trainData, labelsTrain, xtest=testData, ntree=numTrees)
endTime <- Sys.time()
duration = endTime - startTime
duration


predictions <- data.frame(ID=1:nrow(testData), LabelReal=labelsTest, LabelPredicted=levels(labelsTest)[rf$test$predicted], Result=rf$test$predicted==labelsTest)

head(predictions)

errors <- predictions[,4]
table(errors)
fileName <- paste("~/mow/crime/randomforest", numTrees, ".csv", sep="")

write_csv(predictions, fileName) 
duration
