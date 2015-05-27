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
 # linearIndices<-1:(nrow(predictions))+
#    nrow(predictions)*(as.numeric(correct)-1)
  linearIndices<-indexOneValuePerRow(as.numeric(correct), 
                                     ncol(predictions), nrow(predictions))
  return(-mean(log(predictions[linearIndices])))
}


train<-read.csv("train.csv", colClasses=c("numeric", rep("numeric", 93), "factor"))
#train<-read.csv("train.csv", colClasses=c("numeric", rep("factor", 93), "factor"))
train$id<-NULL
#for(i in 1:93)
#{
  #use ordered factors (e.g., 2 < 10)
#  train[[i]]<-ordered(train[[i]], levels=levels(train[[i]])[order(as.numeric(as.character(levels(train[[i]]))))])
#}

PCA<-princomp(train[,2:94])
logPCA<-princomp(log(train[,2:94]+1))


require(RWeka)
probability<-.75
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
data<-data.frame(PCA$scores[,1:ncol(PCA$scores)])
data<-data.frame(logPCA$scores[,1:ncol(logPCA$scores)])
data<-train[,2:94]
data$const<-rep(1, nrow(data))
data$target<-train$target
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
#j48 proba=.75 = 1.14 (72%)
#j48 w/ 10 top PCA dimensions 1.34
#j48 w/ 15 top PCA dimensions 1.25
#j48 w/ all PCA dimensions 1.25 (70%)
#j48 w/ all logPCA dimensions 1.26 (69%)