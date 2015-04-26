setwd("C:/Users/ze6ke/Desktop/kaggle/Otto")
source("cv.r")

#matrix[vertical, horizontal]

multiClassLogLoss<-function(predictions, correct)
{
  linearIndices<-1:(nrow(predictions))+
    nrow(predictions)*(as.numeric(correct)-1)
  return(-mean(log(predictions[linearIndices])))
}


train<-read.csv("train.csv", colClasses=c("numeric", rep("factor", 93), "factor"))
train$id<-NULL
for(i in 1:93)
{
  #use ordered factors (e.g., 2 < 10)
  train[[i]]<-ordered(train[[i]], levels=levels(train[[i]])[order(as.numeric(as.character(levels(train[[i]]))))])
}


#run cross validation
cv.naiveBayes(target~., train)


#execute the prediction one time
callToBuildModel<- function(f, trainData)
{
  return(naiveBayes(f, trainData, laplace=600))
}
callToPredict<- function(model, testData)
{
  return (predict(model, newdata = testData, type = "raw"))
}
   
numberOfFolds<-10
data<-train
#create a vector that shows which fold each element will be used as test data for
slices<-cut(seq(1, nrow(data)), breaks=numberOfFolds, labels=FALSE)[sample(nrow(data))]

i<-1
trainData<-data[slices!=i,]
testData<-data[slices==i,]
trainData$const<-rep(1, nrow(trainData))
#f<-formula(paste("target~", paste(paste("feat", 30:35,sep="_"), collapse="+")), sep="")
#f<-formula(target~const)
f<-formula(target~.)
model<-callToBuildModel(f, trainData)
predictions <- callToPredict(model, testData)
multiClassLogLoss(predictions, testData$target)

#1.94 for no inputs
#29.79 for all fields.

