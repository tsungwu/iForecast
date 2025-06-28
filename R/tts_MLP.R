tts.DeepLearning <-function(y,x=NULL,
                      train.end,
                      arOrder=2,
                      xregOrder=0,
                      type,
                      initial=TRUE) {

library(iForecast)
  dataset = .inputdata(y,x,arOrder,xregOrder,type)

  DF=dataset$dataused
  DF0=dataset$data

  t0=which(rownames(as.matrix(DF))==train.end)

  trainData = window(DF,start=start(DF),end=train.end)

  if(length(unique(trainData$y))<5) {
    trainData=as.data.frame(trainData)
    trainData$y=as.factor(trainData$y)

}

if (isTRUE(initial))  {
  h2o::h2o.init()        # Initialize h2o
  invisible(h2o::h2o.no_progress()) # Turn off progress bars
} else { print("Please execute h2o.init() before you run tts.deeplearning")}

  # Convert to H2OFrame objects
  train_h2o <- h2o::as.h2o(trainData)

  # Set names for h2o
  x <- setdiff(names(train_h2o), "y")

  ########################################
  ###=== Estimation of deeplearning ===###
  ########################################
  DL <- h2o::h2o.deeplearning(x = x,
                        y = "y",
                        training_frame = train_h2o,
                        hidden = c(5),
                        epochs = 1000,
                        train_samples_per_iteration = -1,
                        reproducible = TRUE,
                        #activation = "Tanh",
                        single_node_mode = FALSE,
                        balance_classes = FALSE,
                        force_load_balance = FALSE,
                        #seed = 23123,
                        tweedie_power = 1.5,
                        score_training_samples = 0,
                        score_validation_samples = 0,
                        stopping_rounds = 0)

  # Extract leader model

  return(list(output=DL,
              arOrder=arOrder,
              dataused=DF,
              data=DF0,
              TD=type,
              train.end=train.end))

}


