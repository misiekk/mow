############### naive bayes  #############################33
library(klaR)
set.seed(1234) 
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.6, 0.4))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
trainLabels <- iris$Species[ind==1]
testLabels <- iris$Species[ind==2]
trainData <- trainData[, -5]
testData <- testData[, -5]

model <- NaiveBayes(trainData,trainLabels, fL=1)
startTime <- Sys.time()
predictions <- predict(model, testData)
endTime <- Sys.time()
duration = endTime - startTime
print("NaiveBayes:")
duration
toWrite <- data.frame(Rownum=names(predictions$class), Label=predictions$class, RealLabel=testLabels)
toWrite<-transform(toWrite, Result=predictions$class==testLabels)
errorsNB<-toWrite[,4]
table(errorsNB)

############### naive bayes factors #############################
library(klaR)
set.seed(1234) 
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.6, 04))
iriss<-iris
iriss<-data.frame(lapply(iris, as.factor))
trainData <- iriss[ind==1,]
testData <- iriss[ind==2,]
trainLabels <- iriss$Species[ind==1]
testLabels <- iriss$Species[ind==2]
trainData <- trainData[, -5]
testData <- testData[, -5]

model <- NaiveBayes(trainData,trainLabels, fL=0)
startTime <- Sys.time()
predictions <- predict(model, testData)
endTime <- Sys.time()
duration = endTime - startTime
print("NaiveBayes:")
duration
toWrite <- data.frame(Rownum=names(predictions$class), Label=predictions$class, RealLabel=testLabels)
toWrite<-transform(toWrite, Result=predictions$class==testLabels)
errorsNB<-toWrite[,4]
table(errorsNB)

###############  aode #############################
library(notnaive)
set.seed(1234) 
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.6, 0.4))
iriss<-data.frame(lapply(iris, as.factor))
trainData <- iriss[ind==1,]
testData <- iriss[ind==2,]
trainLabels <- iriss$Species[ind==1]
testLabels <- iriss$Species[ind==2]
trainData <- trainData[, -5]
testData <- testData[, -5]

model <- aode(trainData,trainLabels, fL=2)
startTime <- Sys.time()
predictions <- predict(model, testData)
endTime <- Sys.time()
duration = endTime - startTime
print("NaiveBayes:")
duration
toWrite <- data.frame(Rownum=names(predictions$class), Label=predictions$class, RealLabel=testLabels)
toWrite<-transform(toWrite, Result=predictions$class==testLabels)
errorsNB<-toWrite[,4]
table(errorsNB)