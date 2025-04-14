tts.autoML <-function(y,x=NULL,
                      train.end,
                      arOrder=2,
                      xregOrder=0,
                      type,
                      max_models = 20,
                      sort_metric="AUTO",
                      stopping_metric = "AUTO") {


  dataset = .inputdata(y,x,arOrder,xregOrder,type)

  DF=dataset$dataused
  DF0=dataset$data

  t0=which(rownames(as.matrix(DF))==train.end)

  trainData = window(DF,start=start(DF),end=train.end)

  if(length(unique(trainData$y))<5) {
    trainData=as.data.frame(trainData)
    trainData$y=as.factor(trainData$y)

}

  h2o::h2o.init()        # Initialize h2o
  invisible(h2o::h2o.no_progress()) # Turn off progress bars
  
  # Convert to H2OFrame objects
  train_h2o <- h2o::as.h2o(trainData)

  # Set names for h2o
  x <- setdiff(names(train_h2o), "y")
  
  ##################################
  ###=== Estimation of autoML ===###
  ##################################

  autoML <-  h2o::h2o.automl(
    x = x,
    y = "y",
    training_frame = train_h2o,
    max_models=max_models,
    sort_metric=sort_metric,
    stopping_metric=stopping_metric)

  # Extract leader model
  output=autoML@leader
  modelsUsed=autoML@leaderboard
  
  return(list(output=output,
              modelsUsed=modelsUsed,
              arOrder=arOrder,
              dataused=DF,
              data=DF0,
              TD=type,
              train.end=train.end))
}


