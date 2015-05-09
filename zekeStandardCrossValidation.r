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
test<-read.csv("test.csv", colClasses=c("numeric", rep("factor", 93)))
#set.seed(27)
#slice<-cut(sample(nrow(train)), breaks=4, labels=FALSE)
#write.csv(data.frame(slice), "slices.csv", row.names=FALSE)

#this bit is important
slices<-read.csv("slices.csv")

train$id<-NULL
test$id<-NULL
for(i in 1:93)
{
  #use ordered factors (e.g., 2 < 10)
  train[[i]]<-ordered(train[[i]], levels=levels(train[[i]])[order(as.numeric(as.character(levels(train[[i]]))))])
  test[[i]]<-ordered(test[[i]], levels=levels(test[[i]])[order(as.numeric(as.character(levels(test[[i]]))))])  
}

#reduce options on the dimensions
for(i in 1:93)
{
  #use ordered factors (e.g., 2 < 10)
  train[[i]]<-factor((train[[i]]>0)+0) #0=0, 1+=1
  test[[i]]<-factor((test[[i]]>0)+0) #0=0, 1+=1
}


#this bit is important
#produce cross validation files
for(i in 1:4)
{
  trainData<-train[slices!=i,]
  testData<-train[slices==i,]
  model<-naiveBayes(target~., trainData, laplace=6)
  predictions <- predict(model, newdata = testData, type = "raw")
  multiClassLogLoss(predictions, testData$target)
  output.filename<-paste(paste("naive bayes prediction", i), ".csv", sep="")
  ID<-1:nrow(predictions)
  write.csv(data.frame(ID, predictions), output.filename, row.names=FALSE)
}

#produce test data file
trainData<-train
testData<-test
testData$garbage<-rep(1, nrow(test))
model<-naiveBayes(target~., trainData, laplace=6)
predictions <- predict(model, newdata = testData, type = "raw")
output.filename<-"naive bayes prediction test data.csv"
ID<-1:nrow(predictions)
write.csv(data.frame(ID, predictions), output.filename, row.names=FALSE)

#target~const = 1.945
#target~. for 0/1 value in all features = 1.75