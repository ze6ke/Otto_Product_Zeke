
cv<-function (f, data, numberOfFolds=10, numberOfRepetitions=1, callToBuildModel, callToPredict)
{
  #f is a function of the form passed to lm (e.g., y~x)
  #data is a dataframe that has a Survived column
  
  #create empty results vectors
  multiClassLogLoss.values<-c()
  #accuracy.values<-c()
  for(k in 1:numberOfRepetitions)
  {
    #create a vector that shows which fold each element will be used as test data for
    slices<-cut(seq(1, nrow(data)), breaks=numberOfFolds, labels=FALSE)[sample(nrow(data))]
    
    for(i in 1:numberOfFolds)
    {
      trainData<-data[slices!=i,]
      testData<-data[slices==i,]
      model<-callToBuildModel(f, trainData)
      predictions <- callToPredict(model, testData)
      
      #store accuracy and auc for this fold
      multiClassLogLoss.values<-c(multiClassLogLoss.values, multiClassLogLoss(predictions, testData$target))
      #accuracy<-sum(testData$Prediction==testData$Survived)/nrow(testData)
      #accuracy.values<-c(accuracy.values, accuracy)
      
    }
  }
  #return(data.frame(accuracy.values, auc.values))
  return(multiClassLogLoss.values)
}



require(e1071)
cv.naiveBayes<-function(f, data, numberOfFolds=10, numberOfRepetitions=1)
{ 
  cv(f, data, numberOfFolds, numberOfRepetitions
     , function(f, trainData)
     {
       return(naiveBayes(f, trainData, laplace=10))
     }
     , function(model, testData)
     {
       return (predict(model, newdata = testData, type = "raw"))
     }
  )
}

cv.glm<-function(f, data, threshold=.5, numberOfFolds=10, numberOfRepetitions=1)
{ 
  cv(f, data, threshold, numberOfFolds, numberOfRepetitions
     ,  function(f, trainData)
     {
       return(glm(f, trainData, family=binomial("logit")))
     }
     , function(model, testData)
     {
       return (predict(model, newdata = testData, type = "response"))
     }
  )
}