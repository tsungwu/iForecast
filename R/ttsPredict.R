iForecast <- function(Model,newdata,type) {

   if (type == "recursive" & min(Model$arOrder)== 0) {print("AR Order cannot be 0 for recursive forecasts")

   }  else {


  if (class(Model$output)[1]=="train") {

    prediction  <-  .predictCaret(Model, newdata=newdata,type)

  } else if (class(Model$output)[1]=="H2ORegressionModel") {

    prediction  <-  .predictAutoML(Model, newdata=newdata,type=type)

  } else if (class(Model$output)[1]=="keras.engine.sequential.Sequential") {

    prediction <- .predictLSTM(Model, newdata=newdata,type=type)

  }

       }

  return(prediction)

}



.predictCaret <- function(Model, newdata, type){

  testData <- timeSeries::as.timeSeries(newdata)
  output <- Model$output
  arOrder <- Model$arOrder
  Y.check=Model$data[,1]

  if (type=="staticfit") {
  # Static multistep forecasting by direct fit
    if (max(diff(unique(Y.check)))==min(diff(unique(Y.check)))) {
      static.pred <- as.matrix(as.integer(predict(output,testData,type="raw"))-1)
      colnames(static.pred) <- "class"
      static.pred.prob <-predict(output,testData,type="prob")
  colnames(static.pred.prob)=factor(seq(length(levels(as.factor(Y.check))))-1)
      static.pred=cbind(static.pred,static.pred.prob )
      } else {
  static.pred <- as.matrix(predict(output,testData))
  colnames(static.pred) <- "staticfit"
  }

  rownames(static.pred) <- rownames(testData)
  prediction <- timeSeries::as.timeSeries(static.pred)


  } else if (type=="recursive") {  # Recursive Forecasts

    if (min(arOrder) == 0L) {print("AR Order cannot be 0 for recursive forecasts.")

    } else {
  DF0 <- Model$data
  dateID <- as.character(time(DF0))
  test.start <- dateID[which(dateID==as.character(start(testData)))-1]
  test.end <- end(testData)

    ARX=window(DF0,start=test.start,end=test.end)

    ar.names=names(ARX)[grep(names(ARX),pattern="^ar+")]

    LY.names=names(testData)[grep(names(testData),pattern="^ar+")]
    LX.names=names(testData)[-grep(names(testData),pattern="^ar+")]

    plags=length(ar.names)
    ahead=nrow(ARX)

    recursive.pred=NULL
    recursive.pred.prob=NULL


    if (max(diff(unique(Y.check)))==min(diff(unique(Y.check)))) {
      for (i in 1:ahead) {#i=1
      if(length(LX.names)==0) {
        y0=as.numeric(predict(output,ARX[i,],type="raw"))-1
        y0.prob_=predict(output,ARX[i,],type="prob")
      } else {
        y0=as.numeric(predict(output,ARX[i,c(LY.names,LX.names)],type="raw"))-1
        y0.prob_=predict(output,ARX[i,c(LY.names,LX.names)],type="prob")
      }

        recursive.pred.prob=rbind(recursive.pred.prob,y0.prob_)

      if (i < ahead) if (plags==1) {ARX[i+1,ar.names]=y0} else
      { ARX[i+1,ar.names]=c(y0,as.numeric(ARX[i,1:(plags-1)]))}

      recursive.pred=c(recursive.pred,y0)
      }
      recursive.pred=as.matrix(recursive.pred[-1])

    rownames(recursive.pred)=rownames(testData)
    prediction=timeSeries::as.timeSeries(recursive.pred)

  colnames(prediction)="class"

colnames(recursive.pred.prob)=factor(seq(length(levels(as.factor(Y.check))))-1)
prediction=cbind(prediction,recursive.pred.prob[-1,])


      } else {

    for (i in 1:ahead) {#i=1
      if(length(LX.names)==0) {
        y0=as.numeric(predict(output,ARX[i,]))
      } else {
        y0=as.numeric(predict(output,ARX[i,c(LY.names,LX.names)]))
      }

      if (i < ahead) if (plags==1) {ARX[i+1,ar.names]=y0} else
      { ARX[i+1,ar.names]=c(y0,as.numeric(ARX[i,1:(plags-1)]))}

      recursive.pred=c(recursive.pred,y0)
    }
    recursive.pred=as.matrix(recursive.pred[-1])
  rownames(recursive.pred)=rownames(testData)
  prediction=timeSeries::as.timeSeries(recursive.pred)
  colnames(prediction)="recursive"
}
  }
  }


  return(prediction)



}


.predictAutoML <- function(Model,newdata,type){

  testData=timeSeries::as.timeSeries(newdata)
  automl_leader=Model$output
  arOrder=Model$arOrder
  test_h2o=h2o::as.h2o(tibble::as_tibble(testData))

  if (type=="staticfit") {
    # Static multistep forecasting by direct fit
    Pred2.dm=as.matrix(h2o::h2o.predict(automl_leader, newdata = test_h2o))
    static.pred=Pred2.dm
    rownames(static.pred)=rownames(testData)
    colnames(static.pred)="Prediction"
    prediction=timeSeries::as.timeSeries(static.pred)

    colnames(prediction)="staticfit"

  } else if (type=="recursive") {

    # Recursive Forecasts
    # Predict test data: Recursive Forecasts
    if (min(arOrder) == 0L) {print("AR Order cannot be 0 for recursive forecasts.")

    } else {


    DF0=Model$data
    dateID=as.character(time(DF0))
    test.start=dateID[which(dateID==as.character(start(newdata)))-1]
    test.end=end(newdata)

    ARX = h2o::as.h2o(tibble::as_tibble(window(DF0,start=test.start,end=test.end)))
    ahead=nrow(ARX)
    ar.names=names(ARX)[grep(names(ARX),pattern="^ar+")]
    plags=length(ar.names)

    LY.names=names(test_h2o)[grep(names(test_h2o),pattern="^ar+")]
    LX.names=names(test_h2o)[-grep(names(test_h2o),pattern="^ar+")][-1]

    dynPred=NULL
    for (i in 1:ahead) {#i=1

      if(length(LX.names)==0) {y0=h2o::h2o.predict(automl_leader, newdata = ARX[i,LY.names])
      } else {
        y0=h2o::h2o.predict(automl_leader, newdata = ARX[i,c(LY.names,LX.names)])
      }

      if (i < ahead) {if (length(ar.names)==1) { ARX[i+1,ar.names]=y0
      } else {
        updates=merge(y0,ARX[i,ar.names][1:(plags-1)])
        colnames(updates)=ar.names
        updates=h2o::as.h2o(updates)
        ARX[i+1,ar.names]=updates }} else {stop}

      dynPred=rbind(dynPred,as.numeric(as.matrix(y0)))

    }
    prediction=as.matrix(dynPred[-1])
    rownames(prediction)=rownames(testData)
    colnames(prediction)="recursive"
    prediction=timeSeries::as.timeSeries(prediction)

  }
}
  prediction

  return(prediction)



}



.predictLSTM<- function(Model,newdata,type){

  model=Model$output
  testData=timeSeries::as.timeSeries(newdata)
  arOrder=Model$arOrder
  batch.size=Model$batch.size
  SHAPE=Model$SHAPE
  k=Model$k

  test.new=as.matrix(testData)  #remove date index
  dimnames(test.new)=NULL

  x.test = array(data = test.new[,-1], dim = c(nrow(test.new), SHAPE, k))


  if (type=="staticfit") {
    prediction <- as.matrix(predict(model, x.test, batch_size = batch.size))
    rownames(prediction)=rownames(testData)
    prediction=timeSeries::as.timeSeries(prediction)

    colnames(prediction)="staticfit"

  } else if (type=="recursive") {

    # Recursive Forecasts
    # Predict test data: Recursive Forecasts
    if (min(arOrder) == 0L) {print("AR Order cannot be 0 for recursive forecasts.")

    } else {
    DF0=Model$data
    dateID=as.character(time(DF0))
    test.start=dateID[which(dateID==as.character(start(testData)))-1]
    test.end=end(testData)
    ARX=window(DF0,start=test.start,end=test.end)

    ar.names=names(ARX)[grep(names(ARX),pattern="^ar+")]
    plags=length(ar.names)

    LY.names=names(testData)[grep(names(testData),pattern="^ar+")]
    LX.names=names(testData)[-grep(names(testData),pattern="^ar+")][-1]
    ahead=nrow(ARX)
    rownames(ARX)=NULL

    prediction=NULL
    for (i in 1:ahead) { #i=1

      if (length(LX.names)==0 & length(LY.names)>0) {x.test0 = array(data = ARX[i,LY.names], dim = c(nrow(ARX)-1,SHAPE, 5))
      } else if (length(LY.names)==0 & length(LX.names)>0) {x.test0 = array(data = ARX[i,LX.names], dim = c(nrow(ARX)-1,SHAPE, 5))

      } else if (length(LX.names)>0 & length(LY.names)>0) {
        x.test0 = array(data = ARX[i,c(LY.names,LX.names)], dim = c(nrow(ARX)-1,SHAPE, 5))
      }

      y0=as.matrix(predict(model,x.test0, batch_size = batch.size))[1]

      if (i < ahead) {if (plags==1) { ARX[i+1,ar.names]=y0
      } else {

        ARX[i+1,ar.names]=c(y0,as.numeric(ARX[i,1:(plags-1)])) }}

      prediction=c(prediction,y0)

    }
    prediction=as.matrix(prediction[-1])
    rownames(prediction)=rownames(testData)
    prediction=timeSeries::as.timeSeries(prediction)

    colnames(prediction)="recursive"

    }
  }
  prediction

  return(prediction)



}

