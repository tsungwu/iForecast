#' ttsCaret
#'
#' `ttsCaret()` is now deprecated in order to encourage the users to
#' use [tts.caret()] instead.

ttsCaret<-function(y,x=NULL, method,train.end,arOrder=2,xregOrder=0,
                   type,tuneLength=10,preProcess = NULL,resampling="boot",
                   Number=NULL,Repeat=NULL) {
  .Defunct("tts.caret")
}



ttsAutoML<-function(y,x=NULL,train.end,arOrder=2,xregOrder=0,maxSecs=30) {
  .Defunct("tts.autoML")
}

ttsLSTM<-function(y,x=NULL,train.end,arOrder=1,xregOrder=0,type,
                  memoryLoops=10,shape=NULL,dim3=5,batch.range=2:7,batch.size=NULL) {
  .Defunct("NULL")
}
