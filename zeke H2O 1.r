setwd("C:/Users/ze6ke/Desktop/kaggle/Otto")

#matrix[vertical, horizontal]
indexOneValuePerRow<-function(valList, ncol, nrow)
{
  return (1:(nrow)+
            nrow*(valList-1))
}

multiClassLogLoss<-function(predictions, correct)
{
  linearIndices<-indexOneValuePerRow(correct, ncol(predictions), nrow(predictions))
  return(-mean(log(predictions[linearIndices])))
}

randomString<-function(size)
{
  return (paste(sample(c(LETTERS), size, replace=T), collapse=""))
}

train<-read.csv("train.csv", colClasses=c("numeric", rep("numeric", 93), "factor"))
#train<-read.csv("train.csv", colClasses=c("numeric", rep("factor", 93), "factor"))
train$id<-NULL


library(h2o)
#h2oserver <- h2o.init(ip="oldasus")
h2oserver <- h2o.init(ip="localhost")
#h2o.clusterInfo(localh2o)
#h2o.clusterStatus(localh2o)

slices<-read.csv("slices.csv")
train<-read.csv("train.csv", colClasses=c("integer", rep("numeric", 93), "factor"))
numberOfSlices<-max(slices)

trainingData<-vector(mode="list", numberOfSlices)
trainingData.Hex<-vector(mode="list", numberOfSlices)
for(i in 1:numberOfSlices)
{
  
  trainingData[[i]]<-list(train=train[slices!=i,], test=train[slices==i,])
  trainingData[[i]][[1]]<-trainingData[[i]][[1]][sample(1:nrow(trainingData[[i]][[1]])),]
  trainingData[[i]][[2]]<-trainingData[[i]][[2]][sample(1:nrow(trainingData[[i]][[2]])),]
  trainingData.Hex[[i]]<-list(as.h2o(h2oserver, trainingData[[i]][[1]], key=paste("fold", i, "train", sep="")), 
                              as.h2o(h2oserver, trainingData[[i]][[2]], key=paste("fold", i, "test", sep="")))
}

activationOptions<-list("TanhWithDropout", "RectifierWithDropout", "MaxoutWithDropout")
hiddenLayerOptions<-list(c(7), c(17), c(35, 17), c(36, 18), c(36, 36), c(36, 36, 36), c(100, 50), c(100, 100), c(100, 20), c(200, 100, 50), c(500, 250, 50), c(500, 500, 250, 100))
dropoutOptions<-list(0, .1, .2, .5, .7)
inputDropoutOptions<-list(0, .01, .02, .05, .1, .2, .5)
epochsOptions<-list(5, 10, 25, 50,100)#, 200)  #400 was removed because 50 seemed to be plenty
trainSamplesPerIterationOptions<-list(-2, 0, 1, 100, 1000, 10000)
rhoOptions<-list(.95, .98, .99, .999, .9999)
epsilonOptions<-list(1e-9, 1e-8, 1e-7, 1e-6)
l1Options<-list(0, 1e-5, 1e-4, 1e-3, 1e-2)
l2Options<-list(0, 1e-5, 1e-4, 1e-3, 1e-2)
maxw2Options<-list(.1, 1, 10, 100, 1000)
#lossOptions Options aren't well defined for what to put in r
balance_classesOptions<-list(T, F)

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

  logLoss<-rep(NA_real_, numberOfSlices)
  accuracy<-rep(NA_real_, numberOfSlices)
  configTitle<-randomString(4)
  for(fold in 1:numberOfSlices)
  {
    modelKey<-paste("k", configs, "_",fold, configTitle, sep="")
    model<-h2o.deeplearning(x=2:94,
                            y=95,
                            data=trainingData.Hex[[fold]][[1]],
                            validation=trainingData.Hex[[fold]][[2]],
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
                            balance_classes=balance_classes
                            )    
    model<-h2o.getModel(h2oserver, modelKey)
    predictions.Hex<-h2o.predict(model, trainingData.Hex[[fold]][[2]])
    predictionsRaw<-as.data.frame(predictions.Hex)
    predictions<-as.matrix(predictionsRaw[2:10])
    predictedClasses<-apply(predictions, 1, function(x){which.max(x)})
    correct<-as.integer(trainingData[[fold]][[2]][[95]])
    logLoss[fold]<-multiClassLogLoss(predictions, correct)
    accuracy[fold]<-sum(predictedClasses==correct)/length(correct)
  }
  results<-rbind(results, data.frame(activation, hiddenLayers=paste(hiddenLayers, collapse=";"), droptout=dropout[1], epochs, trainSamplesPerIteration, rho, l1, l2, maxw2,
                            balance_classes, 
                            logLoss1=logLoss[1], logLoss2=logLoss[2], logLoss3=logLoss[3], logLoss4=logLoss[4], 
                            accuracy1=accuracy[1], accuracy2=accuracy[2], accuracy3=accuracy[3], accuracy4=accuracy[4]))
}
results
write.csv(results, file="results.csv", append=T, col.names=F)
#  train[,i] <- sqrt(train[,i]+(3/8))
#h2o.shutdown(localh2o)

