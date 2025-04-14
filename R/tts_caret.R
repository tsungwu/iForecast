tts.caret <- function(y,x=NULL, method,train.end,arOrder=2,xregOrder=0,
                     type,tuneLength=10,preProcess = NULL,resampling="boot",
                     Number=NULL,Repeat=NULL) {

dataset=.inputdata(y,x,arOrder,xregOrder,type)

DF=dataset$dataused
DF0=dataset$data

trainData=window(DF,start=start(DF),end=train.end)

if (length(unique(trainData$y))<5) {
rownames(trainData)=NULL
trainData=as.data.frame(trainData)
trainData$y=as.factor(trainData$y)
}

  eq=as.formula("y~.")
  resampling=resampling
  if (method == "svm") {
    ### finding optimal value of a tuning parameter
    sigDist <- kernlab::sigest(eq,data = trainData, frac = 0.5)
    ### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
    svmTuneGrid <- data.frame(.sigma = rep(sigDist[2],10), .C = 2^(-2:7))
#    set.seed(1056)
        output <- caret::train(eq,data = trainData,
                          method = "svmRadial",
                          preProcess = preProcess,
                          tuneGrid = svmTuneGrid,
                          trControl = trainControl(
                          method = resampling,
                          number= if (is.null(Number)) {ifelse(resampling=="cv", 10, 25)} else {Number},
                          repeats= if (is.null(Repeat)) {ifelse(resampling=="repeatedcv", 1, NA)} else {Repeat},
                          savePredictions = TRUE)
                          )

  } else if (method == "gbm") {
    gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                            n.trees = seq(50, 750, by = 50),
                            shrinkage = c(0.01, 0.1),
                           n.minobsinnode = 15)

    output <- caret::train(eq,data = trainData,
                           method = method,
                           preProcess = preProcess,
                           tuneGrid = gbmGrid,
                           verbose=FALSE,
                           trControl = trainControl(
                           method = resampling,
                           number= if (is.null(Number)) {ifelse(resampling=="cv", 10, 25)} else {Number},
                           repeats= if (is.null(Repeat)) {ifelse(resampling=="repeatedcv", 1, NA)} else {Repeat},
                           savePredictions = TRUE)
                           )

      } else if (method %in% c("xgbTree","xgbDART","xgbLinear")) {
        output <- caret::train(y~ ., data = trainData, method=method,
                          preProcess = preProcess,
                          trControl = trainControl(
                          method = resampling,
                          number= if (is.null(Number)) {{ifelse(resampling=="cv", 10, 25)}} else {Number},
                          repeats=if (is.null(Repeat)) {ifelse(resampling=="repeatedcv", 1, NA)} else {Repeat},
                          savePredictions = TRUE),
                          tuneLength = ifelse(resampling== "none", 1, tuneLength)
                          )
      } else {output <- caret::train(y~ ., data = trainData, method=method,
                                     preProcess = preProcess,
                                     trace=FALSE,
                                     trControl = trainControl(
                                       method = resampling,
                                       number= if (is.null(Number)) {{ifelse(resampling=="cv", 10, 25)}} else {Number},
                                       repeats=if (is.null(Repeat)) {ifelse(resampling=="repeatedcv", 1, NA)} else {Repeat},
                                       savePredictions = TRUE),
                                     tuneLength = ifelse(resampling== "none", 1, tuneLength)
      )
      } 
  
  

  trained.Pred=output$pred[order(output$pred$rowIndex),]

  return(list(output=output,
              arOrder=arOrder,
              training.Pred=trained.Pred,
              dataused=DF,
              data=DF0,
              TD=type,
              train.end=train.end))

}


