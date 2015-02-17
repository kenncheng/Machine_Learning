# Question 2 
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(Hmisc)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(1:nrow(training), CompressiveStrength, data=training)
qplot(1:nrow(training), CompressiveStrength, colour=cut2(Cement, g=4), data=training)
qplot(1:nrow(training), CompressiveStrength, colour=cut2(FlyAsh, g=4), data=training)
qplot(1:nrow(training), CompressiveStrength, colour=cut2(Water, g=4), data=training)
qplot(1:nrow(training), CompressiveStrength, colour=cut2(BlastFurnaceSlag, g=4), data=training)
qplot(1:nrow(training), CompressiveStrength, colour=cut2(Superplasticizer, g=4), data=training)
qplot(1:nrow(training), CompressiveStrength, colour=cut2(CoarseAggregate, g=4), data=training)
qplot(1:nrow(training), CompressiveStrength, colour=cut2(Age, g=4), data=training)

# Question 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer)
hist(log(training$Superplasticizer)+1)
qplot(Superplasticizer, data=training)
qplot(log(Superplasticizer)+1, data=training)



# Question 5 answer
library(caret)
library(AppliedPredictiveModeling)
install.packages("e1071")
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
trainData = training[, c(1, 58:69)]
testData = testing[, c(1, 58:69)]

preProc <- preProcess(trainData[,-1], method="pca", thresh=0.8)
preProc # answer to question 4 
trainPC <- predict(preProc, trainData[,-1])
modelFitPCA <- train(trainData$diagnosis ~., method="glm", data=trainPC)
testPC <- predict(preProc, testData[,-1])
confusionMatrix(testData$diagnosis, predict(modelFitPCA, testPC))

preProc <- preProcess(trainData[,-1], method="pca", pcaComp=12)
trainPC <- predict(preProc, trainData[,-1])
modelFitAll <- train(trainData$diagnosis ~., method="glm", data=trainPC)
testPC <- predict(preProc, testData[,-1])
confusionMatrix(testData$diagnosis, predict(modelFitAll, testPC))