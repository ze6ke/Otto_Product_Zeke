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
h2oserver <- h2o.init(ip="localhost", max_mem_size="3g")
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


trainingData<-lapply(integer(length(modificationList)), function(...) vector(mode="list", numberOfSlices))
trainingData.Hex<-lapply(integer(length(modificationList)), function(...) vector(mode="list", numberOfSlices))

modificationList<-c("raw", "sqrt", "sqrtPlus3Over8", "log")
names(trainingData)<-modificationList
names(trainingData.Hex)<-modificationList
for(j in 1:length(modificationList))
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

activationOptions<-list("TanhWithDropout", "RectifierWithDropout", "RectifierWithDropout", "MaxoutWithDropout")#double up on the one that appears the best
hiddenLayerOptions<-list(c(7), c(17), c(35, 17), c(36, 18), c(36, 36), c(36, 36, 36), c(100, 50), c(100, 100), c(100, 20), c(200, 100, 50), c(500, 250, 50), c(500, 500, 250, 100), c(1024, 512, 128))
dropoutOptions<-list(0, .1, .2, .5, .7)
inputDropoutOptions<-list(0, .01, .02, .05, .1, .2, .5)
epochsOptions<-list(5, 5, 10, 10, 15, 25, 50)#,100)#, 200)  #400 was removed because 10 seemed to be plenty
trainSamplesPerIterationOptions<-list(-2)#, 0, 1, 100, 1000, 10000)
rhoOptions<-list(.95, .98, .99, .999, .9999)
epsilonOptions<-list(1e-9, 1e-8, 1e-7, 1e-6)
l1Options<-list(0, 1e-5, 1e-4, 1e-3, 1e-2)
l2Options<-list(0, 1e-5, 1e-4, 1e-3, 1e-2)
maxw2Options<-list(1, 10, 100) #.1 performs worse and 1000 doesn't make a difference
#lossOptions Options aren't well defined for what to put in r
balance_classesOptions<-list(T, F)
dataTransformationOptions<-relist(modificationList, list(1,2))#list("raw", "sqrt", "log")
lossFunctionOptions<-list("MeanSquare", "CrossEntropy", "Automatic")

results<-NULL
for(configs in 1:100)
{
  activation<-activationOptions[[sample(1:length(activationOptions), 1)]]
  hiddenLayers<-hiddenLayerOptions[[sample(1:length(hiddenLayerOptions), 1)]]
  dropout<-rep(dropoutOptions[[sample(1:length(dropoutOptions), 1)]], length(hiddenLayers))
  inputDropout<-inputDropoutOptions[[sample(1:length(inputDropoutOptions), 1)]]
  epochs<-epochsOptions[[sample(1:length(epochsOptions), 1)]]
  trainSamplesPerIteration<-trainSamplesPerIterationOptions[[sample(1:length(trainSamplesPerIterationOptions), 1)]]
  rho<-rhoOptions[[sample(1:length(rhoOptions), 1)]]
  epsilon<-epsilonOptions[[sample(1:length(epsilonOptions), 1)]]
  l1<-l1Options[[sample(1:length(l1Options), 1)]]
  l2<-l2Options[[sample(1:length(l2Options), 1)]]
  maxw2<-maxw2Options[[sample(1:length(maxw2Options), 1)]]
  balance_classes<-balance_classesOptions[[sample(1:length(balance_classesOptions), 1)]]
  dataTransformation<-dataTransformationOptions[[sample(1:length(dataTransformationOptions), 1)]]
  lossFunction<-lossFunctionOptions[[sample(1:length(lossFunctionOptions), 1)]]
  
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
}
results
#write.table(results, file="results.csv", append=TRUE, col.names=F, sep=",", qmethod="double")
#write.csv(results, file="results.csv", append=TRUE, col.names=F)
#  train[,i] <- sqrt(train[,i]+(3/8))
h2o.shutdown(h2oserver)
y

