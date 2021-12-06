\name{rollingWindows}
\alias{rollingWindows}

\encoding{latin1}

\title{Rolling timeframe for time series anaysis}

\description{
It extracts time stamp from a timeSeries object and separates the time into in-sample training and out-of-sample validation ranges.
}
\usage{
rollingWindows(x,estimation="18m",by = "6m")
}

\arguments{

  \item{x}{ The time series matrix of dependent variable, with \code{timeSeries} or \code{zoo} format.
}


  \item{estimation}{The range of insample estimation period, the default is 18 months(18m), where the k-fold cross-section is performed.

}

 \item{by}{The range of out-of-sample validation/testing period, the default is 6 months(6m).

}

}


\details{

This function is similar to the backtesting framework in portfolio analysis. Rolling windows fixes the origin and  the training sample grows over time, moving windows can be achieved by placing window() on dependent variable at each iteration.
}



\value{

  \item{window}{The time labels of from and to}.}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.

}

\examples{
data(macrodata)
dep=macrodata[,1,drop=FALSE]
ind=macrodata[,-1,drop=FALSE]

timeframe=rollingWindows(dep,estimation="300m",by="6m")
FROM=timeframe$from
TO=timeframe$to

type=c("none","trend","season","both")[1]
models=c("rf","rpart","svm","knn","nnet")[4]
t0=1
# you may change this to "for (to in 1: length(TO))" to perform rolling CV
#output = ttsCaret(y=dep, x=ind, arOrder=c(1), xregOrder=c(1), method=models,
#                   tuneLength =10, train.end=as.character(TO[t0]), type=type)
#if(t0==length(TO)) {
#  testData=window(output$dataused,start=TO[t0],end=end(Y))[-1,]
#} else {testData=window(output$dataused,start=TO[t0],end=TO[t0+1])[-1,]}


#P1=iForecast(Model=output,newdata=testData,type="staticfit") # static forecasts generated
#P2=iForecast(Model=output,newdata=testData,type="recursive") # dynamic forecasts generated

#tail(cbind(testData[,1],P1),10)
#tail(cbind(testData[,1],P2),10)


}

