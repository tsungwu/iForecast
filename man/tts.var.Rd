\name{tts.var}
\alias{tts.var}

\encoding{latin1}

\title{Estimate Vector AutoregRessive model by \code{tts.caret}}

\description{
It estimate VAR model by \code{tts.caret}, and generates an object list for multistep forecasts.
}

\usage{

tts.var(
  data,
  p,
  method,
  train.end,
  type,
  trace=TRUE)

}

\arguments{
  \item{data}{
The time series object of the VAR dataset, for example, \code{timeSeries},\code{xts}, or \code{zoo}. y can be either binary or continuous. Date format must be "%Y-%m-%d".
}

  \item{p}{The lag order as in VAR(p).
}


  \item{method}{The train_model_list of \code{caret}. While using this, make sure that the method allows regression. Methods in c("svm","rf","rpart","gamboost","BstLm","bstSm","blackboost") are feasible.

}


  \item{train.end}{The end date of training data, must be specificed.The default dates of train.start and test.end are the start and the end of input data; and the test.start is the 1-period next of train.end.

}


\item{type}{The time dummies variables. We have four selection:\cr "none"=no other variables,\cr "trend"=inclusion of time dummy, \cr "season"=inclusion of seasonal dummies, \cr "both"=inclusion of both trend and season. No default.

}


\item{trace}{Whether to print the looping information. The defaut is TRUE.}

}


\details{

This function calls \code{tts.caret} of package to execute VAR estimation.
}

\value{

  \item{output}{Output list object  generated.}
  \item{method}{The method used.}
  \item{type}{Type of time dummies used, inherited from type of \code{tts.var}}
  \item{data}{The complete data structure}

}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.

}

\examples{
data(macrodata)
y=timeSeries::as.timeSeries(macrodata[,-1])
VLD=window(y,start="2019-01-01",end=end(y))
#OUT1=tts.var(data=y,
#             p=3,
#             method="enet",
#             train.end="2018-12-01",
#             type=c("none","trend","season","both")[1])

#fcst_ml=iForecast.var(OUT1, n.ahead=nrow(VLD))

}

