setwd("C:/Users/ze6ke/Desktop/kaggle/Otto")
source("cv.r")

#matrix[vertical, horizontal]

multiClassLogLoss<-function(predictions, correct)
{
  linearIndices<-1:(nrow(predictions))+
    nrow(predictions)*(as.numeric(correct)-1)
  return(-mean(log(predictions[linearIndices])))
}


#train<-read.csv("train.csv", colClasses=c("numeric", rep("numeric", 93), "factor"))
train<-read.csv("train.csv", colClasses=c("numeric", rep("factor", 93), "factor"))
train$id<-NULL
for(i in 1:93)
{
  #use ordered factors (e.g., 2 < 10)
  train[[i]]<-ordered(train[[i]], levels=levels(train[[i]])[order(as.numeric(as.character(levels(train[[i]]))))])
}

#reduce options on the dimensions
for(i in 1:93)
{
  #use ordered factors (e.g., 2 < 10)
  #train[[i]]<-factor((train[[i]]>0)+(train[[i]]>1)) #0=0, 1=1, 2+=2
  train[[i]]<-factor((train[[i]]>0)+0) #0=0, 1+=1
}


#test
cv.naiveBayes(target~., train)


callToBuildModel<- function(f, trainData)
{
  return(naiveBayes(f, trainData, laplace=6))
}
callToPredict<- function(model, testData)
{
  return (predict(model, newdata = testData, type = "raw"))
}


numberOfFolds<-10
data<-train
data$const<-rep(1, nrow(data))#add a constant to the dataframe so I can test zeroR easily.
#create a vector that shows which fold each element will be used as test data for
slices<-cut(seq(1, nrow(data)), breaks=numberOfFolds, labels=FALSE)[sample(nrow(data))]

i<-1
trainData<-data[slices!=i,]
testData<-data[slices==i,]
model<-callToBuildModel(target~., trainData)
#model<-callToBuildModel(target~const, trainData)
predictions <- callToPredict(model, testData)
multiClassLogLoss(predictions, testData$target)

#target~const = 1.945
#target~. for 0/1 value in all features = 1.75