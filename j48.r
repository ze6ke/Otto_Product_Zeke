setwd("C:/Users/ze6ke/Desktop/kaggle/Otto")
source("cv.r")

#matrix[vertical, horizontal]
indexOneValuePerRow<-function(valList, ncol, nrow)
{
  return (1:(nrow)+
            nrow*(valList-1))
}

multiClassLogLoss<-function(predictions, correct)
{
  linearIndices<-1:(nrow(predictions))+
    nrow(predictions)*(as.numeric(correct)-1)
  return(-mean(log(predictions[linearIndices])))
}


train<-read.csv("train.csv", colClasses=c("numeric", rep("numeric", 93), "factor"))
#train<-read.csv("train.csv", colClasses=c("numeric", rep("factor", 93), "factor"))
train$id<-NULL

probability<-.75

require(RWeka)
cv.J48<-function(f, data, numberOfFolds=10, numberOfRepetitions=1)
{ 
  cv(f, data, numberOfFolds, numberOfRepetitions
     , function(f, trainData)
     {
       return(J48(f, trainData))
     }
     , function(model, testData)
     {  
       #assign a constatnt probability to the predicted class and make the other
       #classes equal and such that the total probability is 1.
       probForPrediction<-probability
       probForNonPrediction<-(1-probForPrediction)/8
       
       prediction<-predict(model, newdata = testData)
       returnVal<-matrix(rep(probForNonPrediction, length(prediction)*9), ncol=9)
       returnVal[indexOneValuePerRow(as.numeric(prediction), ncol=ncol(returnVal), nrow=nrow(returnVal))]<-probability
       return(returnVal)
     }
  )
}

cv.J48(target~., train)


callToBuildModel<-function(f, trainData)
{
  return(J48(f, trainData))
}
callToPredict<-function(model, testData)
{
  probForPrediction<-probability
  probForNonPrediction<-(1-probForPrediction)/8
  
  prediction<-predict(model, newdata = testData)
  returnVal<-matrix(rep(probForNonPrediction, length(prediction)*9), ncol=9)
  returnVal[indexOneValuePerRow(as.numeric(prediction), ncol=ncol(returnVal), nrow=nrow(returnVal))]<-probability
  return(returnVal)
}


numberOfFolds<-10
data<-train
data$const<-rep(1, nrow(data))
#create a vector that shows which fold each element will be used as test data for
slices<-cut(seq(1, nrow(data)), breaks=numberOfFolds, labels=FALSE)[sample(nrow(data))]

i<-1
trainData<-data[slices!=i,]
testData<-data[slices==i,]
model<-callToBuildModel(target~., trainData)
#model<-callToBuildModel(target~const, trainData)
predictions <- callToPredict(model, testData)
multiClassLogLoss(predictions, testData$target)


#calculate accuracy
prediction<-predict(model, newdata = testData)
sum(prediction==testData$target)/length(prediction)

#target~const = 1.945
#j48 proba=.75 = 1.14