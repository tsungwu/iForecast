rollingWindows <- function (x, estimation = "18m", by = "1m") {

  if(is.ts(x) | timeSeries::is.timeSeries(x)) {
    x=timeSeries::as.timeSeries(x)

  } else if(xts::is.xts(x) | zoo::is.zoo(x)) {
    dateID=as.character(as.Date(time(x)))
    x=as.matrix(x)
    rownames(x)=NULL
    rownames(x)=dateID
    x=timeSeries::as.timeSeries(x)
  } else {print("time series object must be among: ts, timeSeries, xts, and zoo")
}
  
  
  period = estimation
  by = by
  periodLength = as.numeric(substr(period, 1, nchar(period) -1))
  periodUnit = substr(period, nchar(period), nchar(period))
  byLength = as.numeric(substr(by, 1, nchar(by) - 1))
  byUnit = substr(by, nchar(by), nchar(by))
  stopifnot(periodUnit %in% c("w", "m","d","q"))
  stopifnot(byUnit %in% c("w", "m","d","q"))
  stopifnot(periodUnit == byUnit)
  positions = as.Date(rownames(x))

  if (diff(range(lubridate::month(x)))==9 & periodUnit == "q") {
    startPositions = unique(as.character(timeDate::timeFirstDayInQuarter(positions)))
    endPositions = unique(as.character(timeDate::timeFirstDayInQuarter(positions)))
    
  } else if (diff(range(lubridate::month(x)))==11 & periodUnit == "m") {
    startPositions = unique(as.character(timeDate::timeFirstDayInMonth(positions)))
    endPositions = unique(as.character(timeDate::timeFirstDayInMonth(positions)))

  } else  if (diff(range(lubridate::day(x)))>20 & periodUnit == "m") {
    startPositions = unique(as.character(timeDate::timeFirstDayInMonth(positions)))
    endPositions = unique(as.character(timeDate::timeLastDayInMonth(positions)))

  } else if (diff(range(lubridate::day(x)))>20 & periodUnit == "w"){
    ID1 = ID2 = NULL
    for (i in unique(lubridate::year(x))) {
      ID1 = c(ID1, !duplicated(subset(lubridate::week(x),
                                      lubridate::year(x) == i)))
      ID2 = c(ID2, !duplicated(subset(lubridate::week(x),
                                      lubridate::year(x) == i),
                               fromLast = TRUE))
    }
    startPositions =  as.character(rownames(as.matrix(x)))[ID1]
    endPositions =  as.character(rownames(as.matrix(x)))[ID2]

  } else {
    ID1 = ID2 = NULL
    for (i in unique(lubridate::year(x))) {
      ID1 = c(ID1, !duplicated(subset(lubridate::yday(x),
                                      lubridate::year(x) == i)))
      ID2 = c(ID2, !duplicated(subset(lubridate::yday(x),
                                      lubridate::year(x) == i), fromLast = TRUE))
    }
    startPositions = as.character(rownames(as.matrix(x)))[ID1]
    endPositions = as.character(rownames(as.matrix(x)))[ID2]

  }


  numberOfPositions = length(startPositions)
  startSeq <- seq(from = 1, to = (numberOfPositions - periodLength +1), by = byLength)
  startDates = as.character(startPositions[startSeq])
  endSeq <- seq(from = periodLength, to = numberOfPositions,
                by = byLength)
  endDates = as.character(endPositions[endSeq])
  windows = list(from = startDates, to = endDates)
  attr(windows, "control") = list(start = start(positions),
                                  end = end(positions), period = period, by = by)
  windows
}



