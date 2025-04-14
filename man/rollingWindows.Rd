\name{rollingWindows}
\alias{rollingWindows}

\encoding{latin1}

\title{Rolling timeframe for time series anaysis}

\description{
It extracts time stamp from a timeSeries object and separates the time into in-sample training and out-of-sample validation ranges.
}
\usage{
rollingWindows(x,estimation="18m",by = "1m")
}

\arguments{

  \item{x}{The time series object with \code{timeSeries}, \code{xts}, or \code{zoo} format of "%Y-%m-%d".
  }

  \item{estimation}{The range of insample estimation period, the default is 18 months(18m), where the k-fold cross-section is performed. Quarter, week and day are also supported (see example).

}

 \item{by}{The range of out-of-sample validation/testing period, the default is 6 months(6m).Quarter, week and day are also supported (see example).

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
y=macrodata[,1,drop=FALSE]
timeframe=rollingWindows(y,estimation="300m",by="6m")
#estimation="300m", because macrodata is monthly
FROM=timeframe$from
TO=timeframe$to

data(ES_Daily)
y=ES_Daily[,1,drop=FALSE]
timeframe=rollingWindows(y,estimation  ="60w",by="1w")
#60 weeks(300+ days) as estimation window and move by 1 week(5+ days).

FROM=timeframe$from
TO=timeframe$to

y=ES_Daily[,1,drop=FALSE]
timeframe=rollingWindows(y,estimation  ="250d",by="10d")
#250-day as estimation window and move by 10 days.


# simulated quarterly data
tmp0=ts(rnorm(800),start=c(1900,1),freq=4)
tmp1=timeSeries::as.timeSeries(tmp0)
tmp2=zoo::as.zoo(tmp0)
tmp3=xts::as.xts(tmp0)
timeframe=rollingWindows(x=tmp3,estimation ="100q",by="12q")
FROM=timeframe$from
TO=timeframe$to


}

