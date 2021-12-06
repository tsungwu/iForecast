ttsCaret <- function(y,x=NULL, method,train.end,arOrder=2,xregOrder=0,type,tuneLength=10,trControl=trainControl(method = "cv")) {

  if (!is.zoo(y)) {print("Data must be a zoo object.")}

  y=timeSeries::as.timeSeries(y)

if (!is.null(x)) {
  x=timeSeries::as.timeSeries(x)
    if ( nrow(y) != nrow(x) ) {print("Variables must have the same rows.")}
}


if (is.null(train.end)) {print("train.end date must be specified.") }

if (is.null(type)) {type="none" }

  train.start=start(y)
  t0=which(as.character(time(y))==train.end)
  test.start=as.character(time(y))[t0+1]
  test.end=as.character(end(y))

p=max(arOrder,xregOrder)
  colNAMES=c(outer(paste0(names(x),"_L"),0:p,FUN=paste0))
if (p==0) {
  y=timeSeries::as.timeSeries(y)
  datasetX=timeSeries::as.timeSeries(x)
  ar0=NULL
} else {
  datasetY=timeSeries::as.timeSeries(embed(y,p+1),time(y)[-c(1:p)])
  y=datasetY[,1,drop=FALSE]
  ar0=datasetY[,-1,drop=FALSE]
  colnames(ar0)=paste0("ar",1:p)

  if (is.null(x)) {datasetX=NULL
  } else {
  datasetX=timeSeries::as.timeSeries(embed(x,p+1),time(x)[-c(1:p)])

  colnames(datasetX)=colNAMES
  }
}

  colnames(y)="y"


  if (min(arOrder)==0) {ar=NULL
      }  else {ar=ar0[,paste0("ar",arOrder)]}



  if (is.null(x)) {X=datasetX} else {
      L.ID=paste0("L",xregOrder)

    IDx=NULL
    for (i in L.ID) {IDx=c(IDx,grep(colNAMES,pattern=i))}
    X=datasetX[,IDx]
  }


  DF <- na.omit(cbind(y,ar,X))


  #4. Dummies for time features
  trend <- 1:nrow(DF)

  if (timeSeries::isRegular(y)) {
  seasonDummy <- data.frame(forecast::seasonaldummy(as.ts(y)))
  DF0 <- cbind(ar0,X,seasonDummy,trend)
  } else {DF0 <- cbind(ar0,X,trend)}


  if (type=="trend") {DF<-cbind(DF,trend)} else if (type=="sesaon") {DF<-cbind(DF,seasonDummy)
  } else if (type=="both") {DF<-cbind(DF,trend,seasonDummy)
  } else {DF <- DF}



  trainData0=window(DF,start=train.start,end=train.end)

  if (max(diff(unique(y)))==min(diff(unique(y)))) {
    trainData=as.data.frame(unclass(trainData0))
      if(is.double(trainData$y)) {
      trainData$y=as.factor(trainData$y)
      levels(trainData$y)=LETTERS[seq(length(levels(trainData$y)))]
      } else {trainData$y=as.factor(trainData$y)}

      } else {trainData=trainData0}

  dep=colnames(DF)[1]

  eq=as.formula(paste(dep,"~."))

  if (method == "svm") {
    ### finding optimal value of a tuning parameter
    sigDist <- kernlab::sigest(eq,data = trainData, frac = 1)
    ### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
    svmTuneGrid <- data.frame(.sigma = rep(sigDist[1],10), .C = 2^(-2:7))
#    set.seed(1056)
          if (max(diff(unique(y)))==min(diff(unique(y)))){
                  output <- caret::train(eq,data = trainData,
                           method = "svmRadial",
                           preProc = c("center", "scale"),
                           tuneGrid = svmTuneGrid,
                           trControl = trainControl(method = "repeatedcv", repeats = 5,
                                                    classProbs =  TRUE))
                  } else {output <- caret::train(eq,data = trainData,
                                                 method = "svmRadial",
                                                 preProc = c("center", "scale"),
                                                 tuneGrid = svmTuneGrid,
                                                 trControl = trainControl(method = "repeatedcv", repeats = 5))

                                                    }
  } else {output <- caret::train(eq, data = trainData, method=method,
                                tuneLength = tuneLength, trControl = trControl)  }

  return(list(output=output,arOrder=arOrder,data=cbind(y,DF0),dataused=DF))

}


