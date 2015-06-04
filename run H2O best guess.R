setwd("C:/Users/ze6ke/Desktop/kaggle/Otto")
#setwd("D:/Users/Ae-young/Desktop/shared")

#matrix[vertical, horizontal]
indexOneValuePerRow<-function(valList, ncol, nrow)
{
  return (1:(nrow)+
            nrow*(valList-1))
}

vectorMax<-function(v1, v2)
{
  return(apply(data.frame(v1, v2), 1, max))
}

multiClassLogLoss<-function(predictions, correct)
{
  minPrediction<-1e-15
  linearIndices<-indexOneValuePerRow(correct, ncol(predictions), nrow(predictions))
  return(-mean(log(vectorMax(predictions[linearIndices], minPrediction))))
}

randomString<-function(size)
{
  return (paste(sample(c(LETTERS), size, replace=T), collapse=""))
}

#train<-read.csv("train.csv", colClasses=c("numeric", rep("numeric", 93), "factor"))
#train<-read.csv("train.csv", colClasses=c("numeric", rep("factor", 93), "factor"))
#train$id<-NULL


library(h2o)
#h2oserver <- h2o.init(ip="oldasus")
h2oserver <- h2o.init(ip="localhost", max_mem_size="3g", nthreads=3)
#h2o.clusterInfo(localh2o)
#h2o.clusterStatus(localh2o)

slices<-read.csv("slices.csv")
raw.train<-read.csv("train.csv", colClasses=c("integer", rep("numeric", 93), "factor"))
numberOfSlices<-max(slices)

sqrt.train<-raw.train
sqrtPlus.train<-raw.train
log.train<-raw.train

for(i in 2:94)
{
  sqrt.train[[i]]<-sqrt(raw.train[[i]])
  sqrtPlus.train[[i]]<-sqrt(raw.train[[i]])+3/8
  log.train[[i]]<-log(raw.train[[i]]+1)
}

train<-list(raw=raw.train, sqrt=sqrt.train, sqrtPlus3Over8=sqrtPlus.train, log=log.train)

trainingData<-vector(mode="list", 3)

modificationList<-c("raw", "sqrt", "sqrtPlus3Over8", "log")

trainingData<-lapply(integer(length(modificationList)), function(...) vector(mode="list", numberOfSlices))
trainingData.Hex<-lapply(integer(length(modificationList)), function(...) vector(mode="list", numberOfSlices))

names(trainingData)<-modificationList
names(trainingData.Hex)<-modificationList
for(j in 1:1)#length(modificationList))
{
  for(i in 1:numberOfSlices)
  {
    trainingData[[j]][[i]]<-list(train=train[[j]][slices!=i,], test=train[[j]][slices==i,])
    for(k in 1:2)
    {
      trainingData[[j]][[i]][[k]]<-trainingData[[j]][[i]][[k]][sample(1:nrow(trainingData[[j]][[i]][[k]])),]
    }
    #trainingData[[j]][[i]][[2]]<-trainingData[[i]][[2]][sample(1:nrow(trainingData[[i]][[2]])),]
    trainingData.Hex[[j]][[i]]<-list(as.h2o(h2oserver, trainingData[[j]][[i]][[1]], key=paste("fold", i, names(trainingData)[j], "train", sep="")), 
                                     as.h2o(h2oserver, trainingData[[j]][[i]][[2]], key=paste("fold", i, names(trainingData)[j], "test", sep="")))
  }
}

results<-NULL
activation<-activationOptions[[sample(1:length(activationOptions), 1)]]
hiddenLayers<-c(100,100)
dropout<-rep(.2, length(hiddenLayers))
inputDropout<-.03
epochs<-25
trainSamplesPerIteration<- -2
#rho<-.95
rho<-.99
epsilon<-1e-8
l1<-1e-4
l2<-1e-5
maxw2<-5
balance_classes<-F
dataTransformation<-"raw"
lossFunction<-"CrossEntropy"

logLoss<-rep(NA_real_, numberOfSlices)
accuracy<-rep(NA_real_, numberOfSlices)
configTitle<-randomString(4)
thisModelsConfig<-data.frame(activation, hiddenLayers=paste(hiddenLayers, collapse=";"), inputDropout, dropout=dropout[1], epochs, trainSamplesPerIteration, 
                             rho, l1, l2, maxw2, epsilon,
                             balance_classes, dataTransformation, lossFunction)
print(thisModelsConfig)
startTime<-Sys.time()
for(fold in 1:numberOfSlices)
{
  modelKey<-paste("k", configs, "_",fold, configTitle, sep="")
  model<-h2o.deeplearning(x=2:94,
                          y=95,
                          data=trainingData.Hex[[dataTransformation]][[fold]][[1]],
                          validation=trainingData.Hex[[dataTransformation]][[fold]][[2]],
                          key=modelKey,
                          classification=T,
                          max_hit_ratio_k=3,
                          score_training_samples=1000,
                          score_interval=60,
                          activation=activation,
                          hidden=hiddenLayers,
                          epochs=epochs,
                          hidden_dropout_ratio=dropout,
                          input_dropout_ratio=inputDropout,
                          train_samples_per_iteration=trainSamplesPerIteration,
                          rho=rho,
                          epsilon=epsilon,
                          l1=l1,
                          l2=l2,
                          max_w2=maxw2,
                          balance_classes=balance_classes,
                          loss=lossFunction
  )
  model<-h2o.getModel(h2oserver, modelKey)
  predictions.Hex<-h2o.predict(model, trainingData.Hex[[dataTransformation]][[fold]][[2]])
  predictionsRaw<-as.data.frame(predictions.Hex)
  predictions<-as.matrix(predictionsRaw[2:10])
  predictedClasses<-apply(predictions, 1, function(x){which.max(x)})
  correct<-as.integer(trainingData[[dataTransformation]][[fold]][[2]][[95]])
  logLoss[fold]<-multiClassLogLoss(predictions, correct)
  accuracy[fold]<-sum(predictedClasses==correct)/length(correct)
}
endTime<-Sys.time()
executionTime<-as.numeric(endTime-startTime, units="mins")/4
thisModelsResults<-data.frame(activation, hiddenLayers=paste(hiddenLayers, collapse=";"), inputDropout, dropout=dropout[1], epochs, trainSamplesPerIteration, 
                              rho, l1, l2, maxw2, epsilon,
                              balance_classes, dataTransformation, lossFunction,
                              executionTime, 
                              logLoss1=logLoss[1], logLoss2=logLoss[2], logLoss3=logLoss[3], logLoss4=logLoss[4], 
                              accuracy1=accuracy[1], accuracy2=accuracy[2], accuracy3=accuracy[3], accuracy4=accuracy[4])
print(thisModelsResults)
write.table(thisModelsResults, file="results.csv", append=TRUE, col.names=F, sep=",", qmethod="double")
results<-rbind(results, thisModelsResults)

results
#write.table(results, file="results.csv", append=TRUE, col.names=F, sep=",", qmethod="double")
#write.csv(results, file="results.csv", append=TRUE, col.names=F)
#  train[,i] <- sqrt(train[,i]+(3/8))
h2o.shutdown(h2oserver)
y
