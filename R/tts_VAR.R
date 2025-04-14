tts.var <- function(data, p, method, train.end, type,trace=TRUE) {
  # Estimation: ML-VAR(p)
  output.ML_VAR=list()
  for (i in 1:ncol(data)) {

    output.ML_VAR[[i]] = tts.caret(y=data[,i], x=data,
                                   arOrder=0,
                                   xregOrder=seq(p),
                                   method,
                                   tuneLength =14,
                                   train.end,
                                   type)
    if (isTRUE(trace)) {
      print(paste0(ncol(data), "-Var VAR(",p,"): ", "Running ",i,"/",(ncol(data)),", ", method))}

  }

  return(list(output=output.ML_VAR,
              method=method,
              type=type,
              data=data))

}



iForecast.var <-function(object,n.ahead) {

  Model<-object$output[[1]]
  DF0 <- Model$data[,-1] #complete data structure
  DF <- Model$dataused[,-1] #used data structure
  data=object$data
  n=ncol(data) #number of variables
  train.end=object$output[[1]]$train.end

  # Create time dummies

  trend0=as.numeric(window(DF0,start=train.end,end=train.end)[,"SS.1"])
  trend=(trend0+1):(trend0+n.ahead)

  if (diff(range(lubridate::day(DF)))<12) { #Regular frequency
    yr=as.numeric(substr(train.end,1,4))
    mon=as.numeric(substr(train.end,6,7))
    freq=ifelse(diff(range(lubridate::month(DF)))==11,12,4)
    fake=ts(rnorm(1),start=c(yr,mon),frequency =freq)
    TD=cbind(SS.1=trend,forecast::seasonaldummy(fake,h=n.ahead))
    TD=as.matrix(TD)
    if (object$type != "none") {
      TD.names=colnames(DF)[-grep(colnames(DF),pattern="_L")]
    }

  } else { # Irregular frequency
    TD=as.matrix(cbind(SS.1=trend))

    if (object$type == "season") {
      stop("Irregular data doesn't allow season")
    } else if (object$type =="both") {
      print("Irregular data doesn't allow season, only trend is included")
      TD.names=colnames(DF)[-grep(colnames(DF),pattern="_L")]
    } else if (object$type =="trend") {
      TD.names=colnames(DF)[-grep(colnames(DF),pattern="_L")]
    }
  }

  Yt=window(data,start=train.end,end=train.end)
  DL=grep(colnames(DF),pattern="_L") #get the position of RHS
  testData0=window(DF,start=train.end,end=train.end)[,DL]
  testData=cbind(Yt,testData0[,-c((max(DL)-(n-1)):max(DL))])

  rownames(testData)=NULL
  names.DL=colnames(DF)[seq(ncol(testData))]
  colnames(testData)=names.DL



  fcst.ML_VAR=NULL
  for(k in 1:n.ahead) {#period-by-period prediction,

    fcst.ML_VAR0=NULL #Given period, Eq-specific prediction
    for (j in 1:length(object$output)) {

      if(object$type=="none") {
        pred=iForecast(Model=object$output[[j]],
                       newdata=testData,
                       Type="static")
      } else {
        pred=iForecast(Model=object$output[[j]],
                       newdata=cbind(testData,t(TD[k,TD.names])),
                       Type="static")
      }

      fcst.ML_VAR0=cbind(fcst.ML_VAR0,pred)

    }

    testData=cbind(fcst.ML_VAR0,testData[1,-c((ncol(testData)-(n-1)):ncol(testData))])
    colnames(testData)=names.DL

    fcst.ML_VAR=rbind(fcst.ML_VAR,fcst.ML_VAR0)
  }

  colnames(fcst.ML_VAR)=colnames(data)

  return(fcst.ML_VAR)
}
