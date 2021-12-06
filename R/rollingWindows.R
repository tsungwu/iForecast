rollingWindows <- function(x,estimation="18m",by = "6m") {
period=estimation
by=by
  periodLength = as.numeric(substr(period, 1, nchar(period) -
                                     1))
  periodUnit = substr(period, nchar(period), nchar(period))
  byLength = as.numeric(substr(by, 1, nchar(by) - 1))
  byUnit = substr(by, nchar(by), nchar(by))
  stopifnot(periodUnit == "m")
  stopifnot(byUnit == "m")
  positions = time(timeSeries::as.timeSeries(x))
  startPositions = unique(positions)
  endPositions = unique(positions)
  numberOfPositions = length(startPositions)
  startSeq <- seq(from = 1, to = (numberOfPositions - periodLength +
                                    1), by = byLength)
  startDates = startPositions[startSeq]
  endSeq <- seq(from = periodLength, to = numberOfPositions,
                by = byLength)
  endDates = endPositions[endSeq]
  windows = list(from = startDates, to = endDates)
  attr(windows, "control") = list(start = start(positions),
                                  end = end(positions), period = period, by = by)
  windows
}


.fillValueBy <- function(data,replacedBy=NA,value=0) {
  N=ncol(data)
  for (j in 1:N) {
    data[which(abs(data[,j])==value),j]=replacedBy
  }
  return(data)
}
