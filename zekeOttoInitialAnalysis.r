setwd("C:/Users/ze6ke/Desktop/kaggle/Otto")

'%+=%'<-function(l, r){eval.parent(substitute(l<-l+r))}
copyToClipboard<-function(table){write.table(tabl, "clipboard", sep="\t")}

#train.character<-read.csv("train.csv", colClasses=c("numeric", rep("character", 93), "factor"))

#convert the features to factors, ordered by the underlying number
#or possibly sort everything by frequency later?


train<-read.csv("train.csv", colClasses=c("numeric", rep("factor", 93), "factor"))
for(i in 2:94)
{
  #use ordered factors (e.g., 2 < 10)
  train[[i]]<-ordered(train[[i]], levels=levels(train[[i]])[order(as.numeric(as.character(levels(train[[i]]))))])
}

train.numeric<-read.csv("train.csv")
#factor analysis of the training data
factanal(train.numeric[1:10000, 2:94], 1)



#how often do we have values that arent the most common value?
#specifically, for each observations, how many features are non-zero
featureVal1<-apply(train, 1, FUN=function(x){sum(as.numeric(x[2:94])>=1)})
featureVal2<-apply(train, 1, FUN=function(x){sum(as.numeric(x[2:94])>=2)})
featureVal3<-apply(train, 1, FUN=function(x){sum(as.numeric(x[2:94])>=3)})
summary(featureVal1)
summary(featureVal2)
summary(featureVal3)

require(polycor)
#this is fairly slow and test polychoric (factor to factor) correlation
polychor(train[,2:94]=="0")

sumAcrossMargin<-function(myTable, margin)
{
  apply(myTable, margin, function(x){Reduce('+', x)})
}

numberOfClasses<-length(levels(train$target))
countMatrix<-list()
totalsByClass<-list()
probabilityOfClassGivenFeature<-list()

#show all the distributions.
for(feat in 2:94)
{
  countMatrix[[feat]]<-matrix(rep(0,numberOfClasses*length(levels(train[[feat]]))), nrow=numberOfClasses)
  for(i in 1:nrow(train))
  {
    countMatrix[[feat]][as.numeric(train$target[i]),as.numeric(train[[feat]][i])] %+=% 1
  }
  
  
  
  totalsByClass[[feat]]<-sumAcrossMargin(countMatrix[[feat]], 2)
  probabilityOfClassGivenFeature[[feat]]<-apply(countMatrix[[feat]], 1, 
                                             function(x){x/totalsByClass[[feat]]})

  
  #split plot area in two and add a histogram above the density plot
  layout(matrix(data=c(0, 2, 1, 0, 1, 3, 1, 0), nrow=2, ncol=4), 
         widths=c(1,.25, 8, .25), heights=c(1,1))
  #layout.show(3)
  
  #another option to display non-logarithmic histograms
  #hist(as.numeric(train[[feat]]), 
  #     breaks=0:length(levels(train[[feat]])), 
  #     main=paste("Probability of Each Class Given Value of Feature", feat))
  
  #log on y for the histogram or not (y or blank)
  ylog<-"y"
  plot(train[[feat]], log=ylog, type='h', lwd=1, lend=2, 
       main=paste("Probability of Each Class Given Value of Feature", feat))
  
  #display key
  image(1, 0:255, matrix(data=1:255,nrow=1), col=gray.colors(255)[255:1], axes=F)
  axis(2, at=c(0, 255), labels=c("0", "1"))

  #display heatmap of class given the value of this feature
  image(1:length(levels(train[[feat]])), 1:numberOfClasses, 
    probabilityOfClassGivenFeature[[feat]]*255, col=gray.colors(255)[255:1], main="")
  
}

