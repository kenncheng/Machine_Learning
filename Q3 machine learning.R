# Q3 machine learning
# Question 1 answers
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
training <- subset(segmentationOriginal, Case=="Train")
testing <- subset(segmentationOriginal, Case == "Test")
set.seed(125)
modFit <- train(Class ~., method="rpart", data=training)
print(modFit$finalModel)
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 
# Answer PS.WS, PS, Not possible to predict

# Question 2
# The bias is larger and the variance is smaller. Under leave one out cross 
# validation K is equal to the sample size.

# Question 3
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
modFit <- train(Area ~., method="rpart", data=olive)
print(modFit$finalModel)

predict(modFit, newdata=as.data.frame(t(colMeans(olive))))

# Question 4
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
modFit <- train(chd ~ age +alcohol + obesity + tobacco + typea + ldl, 
                method ="glm", family ="binomial", data=trainSA)
prediction <- predict(modFit, newdata=testSA) # prediction using testSA
missClass <- function(values,prediction)
{sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd, prediction)
prediction <- predict(modFit, newdata=trainSA) # prediction using trainSA
missClass(trainSA$chd, prediction)

# Question 5
library(ElemStatLearn)
install.packages("randomforest")
data(vowel.train)
data(vowel.test) 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modFit <- train(y ~., data=vowel.train, method="rf", prox=TRUE)
modFit
varImp(modFit, scale = FALSE)
