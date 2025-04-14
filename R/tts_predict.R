iForecast <- function(Model,newdata=NULL,Type,n.ahead=NULL) {

  if (Type == "dynamic" & max(Model$arOrder)== 0) {print("AR Order cannot be 0 for recursive forecasts")

  }  else {


    if (class(Model$output)[1]=="train") {

      prediction  <-  .predict_caret(Model, newdata,Type=Type,n.ahead)

    } else if (substr(class(Model$output)[1],1,3) == "H2O") {

      prediction  <-  .predict_AutoML(Model, newdata,Type=Type,n.ahead)

    } #else if (class(Model$output)[1]=="keras.engine.sequential.Sequential") {

      #prediction <- .predictLSTM(Model, newdata,type=type)

    #}

  }

  return(prediction)

}

.predict_caret <- function(Model, newdata, Type,n.ahead) {

  DF <- Model$dataused #used data structure
  DF0 <- Model$data #complete data structure
  output <- Model$output
  arOrder <- Model$arOrder
  Y.check=DF[,1,drop=FALSE]
  X.check=grep(colnames(Model$dataused),pattern="_L+")
  train.end=Model$train.end


##== Static model fit
if (Type == "static") {
  testData=newdata

#classification case
    if (length(unique(Y.check)) <5) {
      static.pred <- as.matrix(as.integer(predict(output,testData,type="raw")))
      colnames(static.pred) <- "modelFit"
            if (output$method=="svmRadial") {
              static.pred=static.pred
            } else {
            static.pred.prob <- predict(output,testData,type="prob")
            static.pred=cbind(static.pred,static.pred.prob )
            }

# continuous case
    } else {
      static.pred <- as.matrix(predict(output,testData))
      colnames(static.pred) <- "modelFit"
    }

prediction <- static.pred
rownames(prediction)=rownames(as.matrix(testData))



##== Dynamic/Recursive Forecasts
  } else {

  if (max(arOrder) == 0L | is.null(arOrder)) {stop("arOrder is required for dynamic forecasts.")}

  if (length(X.check)!=0L) {stop("dynamic forecast allows NO covariates, except time dummies.")  }

  if (is.null(n.ahead)) {stop("dynamic forecast must specify steps to forecast ahead")  }

      ar.names=colnames(DF0)[-1][grep(colnames(DF0)[-1],pattern="^ar+")]

      arUSED.names=colnames(DF)[-1][grep(colnames(DF)[-1],pattern="^ar+")]

      ARs <- window(DF0,start=train.end,end=train.end)
      trend0=as.numeric(ARs[,"SS.1"])
      ARs=ARs[,seq(length(ar.names)+1)][,-(length(ar.names)+1)]
      rownames(ARs)=NULL
      colnames(ARs) <-ar.names


      # Create time dummies
      trend=(trend0+1):(trend0+n.ahead)

      if (diff(range(lubridate::day(DF)))<12) { #Regular frequency
        yr=as.numeric(substr(train.end,1,4))
        mon=as.numeric(substr(train.end,6,7))
        freq=ifelse(diff(range(lubridate::month(DF)))==11,12,4)
        fake=ts(rnorm(1),start=c(yr,mon),frequency =freq)
        TD=cbind(SS.1=trend,forecast::seasonaldummy(fake,h=length(trend)))
        TD=as.matrix(TD)
        if (Model$TD != "none") {
        TD.names=colnames(DF)[-1][-grep(colnames(DF)[-1],pattern="^ar+")]
      }

      } else { # Irregular frequency
        TD=as.matrix(cbind(SS.1=trend))

        if (Model$TD == "season") {
          print("Irregular data doesn't allow season")
        } else if (Model$TD =="both") {
          print("Irregular data doesn't allow season, only trend is included")
          TD.names=colnames(DF)[-1][-grep(colnames(DF)[-1],pattern="^ar+")]
        } else if (Model$TD =="trend") {
          TD.names=colnames(DF)[-1][-grep(colnames(DF)[-1],pattern="^ar+")]
        }
      }




## Compute Forecasts
      recursive.pred=NULL
      recursive.pred.prob=NULL
## Classification model
      if (length(unique(Y.check)) <6) { #check if classification

        for (i in 1:n.ahead) {#i=1

          if (diff(range(lubridate::month(DF)))<12) {#Regular frequency
              if (Model$TD == "none") {
                newData=ARs[,arUSED.names]
                if (output$method=="svmRadial") {
                y0=as.numeric(predict(output,newData,type="raw"))
                } else {
                y0=as.numeric(predict(output,newData,type="raw"))
                y0.prob_=predict(output,newData,type="prob")
                }
              } else {
                newData=cbind(ARs[,arUSED.names],t(TD[i,TD.names]))

                if (output$method=="svmRadial") {
                  y0=as.numeric(predict(output,newData,type="raw"))
                  } else {

                  y0=as.numeric(predict(output,newData,type="raw"))
                  y0.prob_=predict(output,newData,type="prob")
                  }
              }

          } else { #Irregular frequency
            if (Model$TD %in% c("none","season")) {
              newData=ARs[,arUSED.names]

              if (output$method=="svmRadial") {

                y0=as.numeric(predict(output,newData,type="raw"))

              } else {
              y0=as.numeric(predict(output,newData,type="raw"))
              y0.prob_=predict(output,newData,type="prob")
              }

            } else {
              newData=cbind(ARs[,arUSED.names],t(TD[i,TD.names]))

              if (output$method=="svmRadial") {
                y0=as.numeric(predict(output,newData,type="raw"))
              } else {

                y0=as.numeric(predict(output,newData,type="raw"))
                y0.prob_=predict(output,newData,type="prob")
              }

            }

          }

if (output$method=="svmRadial") {
            recursive.pred=c(recursive.pred,y0)
          } else {
           recursive.pred.prob=rbind(recursive.pred.prob,y0.prob_)
           recursive.pred=c(recursive.pred,y0)
          }

 ARs=cbind(y0,ARs[,-length(ar.names)])
 colnames(ARs) <-ar.names


} #End of i loop

        if (output$method=="svmRadial") {
          recursive.pred=as.matrix(recursive.pred)
          prediction=recursive.pred
          colnames(prediction)="recursive"

        } else {
        recursive.pred=as.matrix(recursive.pred)
        prediction=recursive.pred

        colnames(prediction)="recursive"

        prediction=cbind(prediction,recursive.pred.prob)
        }



## Continuous case
      } else {
        for (i in 1:n.ahead) { #i=1

          if (diff(range(lubridate::day(DF)))<12) { #Regular frequency
              if (Model$TD == "none") {
                newData=ARs[,arUSED.names]
                y0=as.numeric(predict(output,newData))
              } else {
                newData=cbind(ARs[,arUSED.names],t(TD[i,TD.names]))
              y0=as.numeric(predict(output,newData))
          }

          } else {# Irregular frequency

            if (Model$TD %in% c("none","season")) {
              newData=ARs[,arUSED.names]
              y0=as.numeric(predict(output,newData))
            } else {
              newData=cbind(ARs[,arUSED.names],t(TD[i,TD.names]))
              y0=as.numeric(predict(output,newData))
            }

       }

          recursive.pred=c(recursive.pred,y0)

          ARs=cbind(y0,ARs[,-length(ar.names)])
          colnames(ARs) <-ar.names


  } #End of i loop
        prediction=as.matrix(recursive.pred)
        colnames(prediction)="recursive"
}

  }


  return(prediction)

}








.predict_AutoML <- function(Model,newdata,Type,n.ahead){

  DF <- Model$dataused #used data structure
  DF0 <- Model$data   #complete data structure


  automl_leader=Model$output
  arOrder=Model$arOrder
  Y.check=DF$y
  X.check=grep(colnames(DF),pattern="_L+")
  train.end=Model$train.end

# Static forecasting
if (Type == "static") {

      testData=h2o::as.h2o(newdata)

      #Classification case
      if(length(unique(Y.check))<5) {

        Pred2.dm=as.matrix(h2o::h2o.predict(automl_leader,
                                            newdata = testData))

       prediction=apply(Pred2.dm,2,as.double)
       colnames(prediction)=colnames(Pred2.dm)

       #Continuous case
      } else {
        Pred2.dm=h2o::h2o.predict(automl_leader,
                                  newdata =testData)
        prediction=as.matrix(Pred2.dm)
        colnames(prediction)="Prediction"
        colnames(prediction)="modelFit"
      }

rownames(prediction)=as.character(rownames(newdata))
prediction=timeSeries::as.timeSeries(prediction)
#===== End of static forecast

##== Dynamic/Recursive Forecasts
} else {



    if (max(arOrder) == 0L | is.null(arOrder)) {stop("arOrder is required for dynamic forecasts.")}

    if (length(X.check)!=0L) {stop("dynamic forecast allows NO covariates, except time dummies.")  }

    if (is.null(n.ahead)) {stop("dynamic forecast must specify steps to forecast ahead")  }

    ar.names=colnames(DF0)[-1][grep(colnames(DF0)[-1],pattern="^ar+")]

    arUSED.names=colnames(DF)[-1][grep(colnames(DF)[-1],pattern="^ar+")]

    ARs <- window(DF0,start=train.end,end=train.end)
    trend0=as.numeric(ARs[,"SS.1"])
    ARs=ARs[,seq(length(ar.names)+1)][,-(length(ar.names)+1)]
    rownames(ARs)=NULL
    colnames(ARs) <-ar.names

    # Create time dummies
    trend=(trend0+1):(trend0+n.ahead)
    if (diff(range(lubridate::day(DF)))<12) { #Check Regular frequency
      yr=as.numeric(substr(train.end,1,4))
      mon=as.numeric(substr(train.end,6,7))
      freq=ifelse(diff(range(lubridate::month(DF)))==11,12,4)
      fake=ts(rnorm(1),start=c(yr,mon),frequency =freq)
      TD=cbind(SS.1=trend,forecast::seasonaldummy(fake,h=length(trend)))
      TD=as.matrix(TD)
      if (Model$TD != "none") {
        TD.names=colnames(DF)[-1][-grep(colnames(DF)[-1],pattern="^ar+")]
      }

    } else { # Irregular frequency
      TD=as.matrix(cbind(SS.1=trend))

      if (Model$TD == "season") {
        print("Irregular data doesn't allow season")
      } else if (Model$TD =="both") {
        print("Irregular data doesn't allow season, only trend is included")
        TD.names=colnames(DF)[-1][-grep(colnames(DF)[-1],pattern="^ar+")]
      } else if (Model$TD =="trend") {
        TD.names=colnames(DF)[-1][-grep(colnames(DF)[-1],pattern="^ar+")]
      }
    }



 dynPred=NULL
 for (i in 1:n.ahead) {# start of for loop, i=1
        if (diff(range(lubridate::day(DF)))<12) { #Check Regular frequency
              if(Model$TD == "none") {
                newData=h2o::as.h2o(ARs[,arUSED.names])
                y0=h2o::h2o.predict(automl_leader,newData)
              } else {
                newData=h2o::as.h2o(cbind(ARs[,arUSED.names],t(TD[i,TD.names])))
                y0=h2o::h2o.predict(automl_leader, newData)
              }
        } else { #Irregular frequency

          if(Model$TD %in% c("none","season")) {
            newData=h2o::as.h2o(ARs[,arUSED.names])
            y0=h2o::h2o.predict(automl_leader,newData)
          } else {
            newData=h2o::as.h2o(cbind(ARs[,arUSED.names],t(TD[i,TD.names])))
            y0=h2o::h2o.predict(automl_leader, newData)
          }

        }

        dynPred=rbind(dynPred,as.matrix(y0))

        if (length(unique(Y.check))<5) {
          ARs=cbind(as.numeric(y0[1,1]),ARs[,-length(ar.names)])
          colnames(ARs) <-ar.names

        } else {

        ARs=cbind(y0,ARs[,-length(ar.names)])
        colnames(ARs) <-ar.names}

} #end of for loop


       if (length(unique(Y.check))<5) {
         prediction=apply(dynPred,2,as.double)
         colnames(prediction)=colnames(dynPred)

       } else {
            prediction=as.matrix(dynPred)
            colnames(prediction)="recursive"

            }
 prediction

    } # end of dynamic forecast


return(prediction)

}
