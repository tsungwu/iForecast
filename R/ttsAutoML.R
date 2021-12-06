ttsAutoML <-function(y,x=NULL,train.end,arOrder=2,xregOrder=0,maxSecs=30) {
  if (!is.zoo(y)) {print("The data must be timeSeries object.")}

  Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  h2o::h2o.init()        # Fire up h2o
  invisible(h2o::h2o.no_progress()) # Turn off progress bars

  y=timeSeries::as.timeSeries(y)

  # Load libraries

  if (!is.null(x)) {
    x=timeSeries::as.timeSeries(x)

      if ( nrow(y) != nrow(x) ) {print("Variables must have the same rows.")
      }
  }



  if (is.null(train.end)) {print("The train.end must be specified.") }

  train.start=start(y)
  t0=which(as.character(time(y))==train.end)
  test.start=as.character(time(y))[t0+1]
  test.end=as.character(end(y))

  p=max(arOrder,xregOrder)
  colNAMES=c(outer(paste0(names(x),"_L"),0:p,FUN=paste0))
  if (p==0) {
    y=y
    datasetX=timeSeries::as.timeSeries(x)
    ar0=NULL
  } else {
    datasetY=timeSeries::as.timeSeries(embed(y,p+1),time(y)[-c(1:p)])
    y=datasetY[,1]
    ar0=datasetY[,-1]
    colnames(ar0)=paste0("ar",1:p)

    if (is.null(x)) {datasetX=NULL
    } else {
      datasetX=timeSeries::as.timeSeries(embed(x,p+1),time(x)[-c(1:p)])

      colnames(datasetX)=colNAMES
    }
  }

  if (min(arOrder)==0) {ar=NULL
  }  else {ar=ar0[,paste0("ar",arOrder)]}


  if (is.null(x)) {X=NULL} else {

    L.ID=paste0("L",xregOrder)
    IDx=NULL
    for (i in L.ID) {IDx=c(IDx,grep(colNAMES,pattern=i))}
    X=datasetX[,IDx]

    }


  colnames(y)="y"

  ### The beginning of autoML


  # Augment (adds data frame columns)
  if (max(lubridate::hour(y)) == min(lubridate::hour(y))) {
  timeID=data.frame(date=as.Date(as.character(time(y))))
  suppressMessages(timeFeatures <- timetk::tk_augment_timeseries_signature(timeID))
  timeFeatures=as.data.frame(timeFeatures[,-1])
  rownames(timeFeatures)=as.character(time(y))
  timeFeatures=timeSeries::as.timeSeries(timeFeatures)
  dateID=as.character(as.Date(time(y)))[-1]
  data_tbl_aug=tibble::as_tibble(na.omit(cbind(y,ar,timeFeatures))) #The first obs is lost
} else {
  timeFeatures=NULL
  dateID=as.character(rownames(y))
  data_tbl_aug=tibble::as_tibble(na.omit(cbind(y,ar))) #The first obs is lost
}

if (!is.null(x)) {data_tbl_aug=cbind(data_tbl_aug,X[-1,])}
  
  data_tbl_clean=data_tbl_aug

  data_tbl_clean=as.data.frame(data_tbl_clean)
  rownames(data_tbl_clean)=dateID

  DF=timeSeries::as.timeSeries(data_tbl_clean)
  trainData = window(DF,start=train.start,end=train.end)
  testData = window(DF,start=test.start,end=test.end)

  # Convert to H2OFrame objects
  train_h2o <- h2o::as.h2o(tibble::as_tibble(trainData))
  test_h2o  <- h2o::as.h2o(tibble::as_tibble(testData))

  # Set names for h2o
  x <- setdiff(names(train_h2o), "y")
  ##################################
  ###=== Estimation of autoML ===###
  ##################################

  automl_models_h2o <-  h2o::h2o.automl(
    x = x,
    y = "y",
    training_frame = train_h2o,
    leaderboard_frame = test_h2o,
    max_runtime_secs = maxSecs,
    stopping_metric = "deviance")

  # Extract leader model
  automl_leader <- automl_models_h2o@leader

  DF0=tibble::as_tibble(na.omit(cbind(y,ar0, timeFeatures,X))) #Short of the first obs
  DF0 = as.data.frame(DF0)
  rownames(DF0)=dateID
  DF0=timeSeries::as.timeSeries(DF0)

  return(list(output=automl_leader,arOrder=arOrder,data=DF0,dataused=DF))
}


