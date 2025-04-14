.inputdata <- function(y,x, arOrder,xregOrder, type) {

  if (is.null(x)) {
    stopifnot(timeSeries::is.timeSeries(y) | is.ts(y) | zoo::is.zoo(y)| xts::is.xts(y))
} else {
  stopifnot(timeSeries::is.timeSeries(x) | is.ts(x) | zoo::is.zoo(x)| xts::is.xts(x))
  stopifnot(timeSeries::is.timeSeries(y) | is.ts(y) | zoo::is.zoo(y)| xts::is.xts(y))

  if(is.ts(x) | timeSeries::is.timeSeries(x)) {
    x=timeSeries::as.timeSeries(x)
  } else {
    x=timeSeries::timeSeries(x,as.POSIXct(x))
  }

  }



  if(is.ts(y) | timeSeries::is.timeSeries(y)) {
    y=timeSeries::as.timeSeries(y)
  } else {
    y=timeSeries::timeSeries(y,as.POSIXct(y))
  }


  if (is.null(x)) {x=NULL} else if (nrow(y) != nrow(x)) {stop("Variables must have the same rows.")}

  if (is.null(type)) {type="none" }


#Step 1. created Y dataset
  if (max(arOrder) == 0L | is.null(arOrder)) {
    datasetY=timeSeries::as.timeSeries(y)
    colnames(datasetY)="y"
    ar.names=NULL

  } else {
    datasetY=timeSeries::as.timeSeries(embed(y,max(arOrder)+1),
                                       rownames(as.matrix(y))[-c(1:max(arOrder))])

    colnames(datasetY)=c("y",paste0("ar",1:max(arOrder))) #full Y dataset

    ar.names=paste0("ar",arOrder) #used AR names
  }

#Step 2. created X dataset
  if (is.null(x)) {
      datasetX=NULL
      xnames=NULL

  } else {
    if (max(xregOrder) == 0L | is.null(xregOrder)) { #X has no lags
      datasetX=timeSeries::as.timeSeries(x)
      colnames(datasetX)=paste0(names(x),"_L",0)
      xnames=paste0(names(x),"_L",0)
    } else {

  datasetX=timeSeries::as.timeSeries(embed(x,max(xregOrder)+1),
                                     rownames(as.matrix(x))[-c(1:max(xregOrder))]) #full X dataset
  colnames(datasetX)=c(outer(paste0(names(x),"_L"),0:max(xregOrder),FUN=paste0))
  xnames=c(outer(paste0(names(x),"_L"),xregOrder,FUN=paste0)) #used X colnames
}
  }


#Step 3. Bind dataset
if(is.null(x)) {

    DF0=na.omit(timeSeries::as.timeSeries(datasetY)) #Complete dataset

    if (max(arOrder) == 0L | is.null(arOrder)) {
    DF <- DF0[,"y"]
    } else { DF <- DF0[,c("y",ar.names)] } #Used dataset

} else {

      DF0=na.omit(timeSeries::as.timeSeries(cbind(datasetY,datasetX))) #Complete dataset

      if (max(arOrder) == 0L | is.null(arOrder)) {
        DF <- DF0[,c("y",xnames)]
      } else {DF <- DF0[,c("y",ar.names,xnames)]} #Used dataset

}

#Step 4. Dummies for time features
  trend <- seq(nrow(y))

  if (diff(range(lubridate::day(DF)))<12) { #regular data
    yr=as.numeric(substr(rownames(as.matrix(DF))[1],1,4))
    mon=as.numeric(substr(rownames(as.matrix(DF))[1],6,7))
    freq=ifelse(diff(range(lubridate::month(DF)))==11,12,4)
    fake=ts(rep(1,nrow(DF)),start=c(yr,mon),frequency =freq)
    seasonDummy <- as.matrix(forecast::seasonaldummy(fake))
  } else {seasonDummy=NULL
#  print("Irregular frequency does not have seasonal dummies")
  }

  DF=timeSeries::as.timeSeries(DF)
  DF0=timeSeries::as.timeSeries(DF0)

  DF0<-cbind(DF0,trend,seasonDummy)


  if (diff(range(lubridate::day(DF)))<12) { #Check if regular
    if (type=="season") {
      DF<-cbind(DF,seasonDummy)
      } else if (type=="trend") {
        DF<-cbind(DF,trend)
      } else if (type=="both") {
        DF<-cbind(DF,trend,seasonDummy)
      } else {DF=DF}

  } else {
      if (type=="trend") {
        DF<-cbind(DF,trend)
      } else if (type=="season") {
        print("Irregular frequency does not allow seasonal dummies")
        DF<-DF
      } else if (type=="both") {
        print("Irregular frequency does not allow seasonal dummies")
        DF<-cbind(DF,trend)
      } else {DF=DF}
 }


  return(list(data=DF0, dataused=DF))

}



Accuracy <- function(f, x) {

  error <- (x - f)
  pe <- error/x

  mse <- mean(error ^ 2, na.rm = TRUE)
  mae <- mean(abs(error), na.rm = TRUE)
  mape <- mean(abs(pe), na.rm = TRUE)
  maape <- mean(atan(abs(pe)), na.rm = TRUE)


  fpe <- c(f[2:nrow(x)])/c(x[1:(nrow(x) - 1)]) - 1
  ape <- c(x[2:nrow(x)])/c(x[1:(nrow(x) - 1)]) - 1
  theil <- sqrt(sum((fpe - ape) ^ 2, na.rm = TRUE) / sum(ape ^ 2, na.rm = TRUE))
  rho <- acf(error, plot = FALSE, lag.max = 2, na.action = na.pass)$acf[2, 1, 1]

  measure <- cbind(sqrt(mse), mae, mape, maape, rho, theil)
  colnames(measure) <- c("RMSE", "MAE", "MAPE","MAAPE","ACF1", "TheilU")
  rownames(measure) <- "values"
  return(measure)
}



