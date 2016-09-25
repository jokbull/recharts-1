

#' add Series
#' @export
addSeries = function(chart,data,...){
  if (is.vector(data))  {
    .addSeries.vector(chart,data,...)
  } else if (is.data.frame(data)) {
    .addSeries.data.frame(chart,data,...)
  } else if (is.list(data)) {
    .addSeries.list(chart, data, ...)
  }
}

.addSeries.data.frame = function(chart, newSeriesDF,...) {
  res = list(...)
  if (!hasArg(type)) {
    res$type = "line"
  }
  if (hasArg(yAxis)) {
    attr(res,"yAxis") = yAxis
  }
  res$data = as.matrix(newSeriesDF[,1:2])
  .addSeries.list(chart, res)
}


.addSeries.vector = function(chart, newSeriesVec, ...) {
  res = list(...)
  if (!hasArg(type)) {
    res$type = "line"
  }
  if (hasArg(yAxis)) {
    attr(res,"yAxis") = yAxis
  }

  if ("type" %in% names(chart$x$xAxis)) {
    if ("data" %in% names(chart$x$xAxis)) {
      XAxisHaveData = TRUE
    } else {
      XAxisHaveData = FALSE
    }
  } else {
    stop("PLEASE ASSIGN WHICH XAXIS IS USED")
  }

  if (!XAxisHaveData) {
    x = attr(newSeriesVec,"x")
    if (is.null(x)) {
      stop("PLEASE INCLUDE x Data")
    }
    plotData = cbind(x,newSeriesVec)
  } else {
    plotData = newSeriesVec
  }
  res$data = plotData
  .addSeries.list(chart, res)
}


.addSeries.list = function(chart, newSeries, ...) {
  stopifnot(is.list(newSeries))

  itemNames = names(newSeries)
  stopifnot("data" %in% itemNames)

  if (!"type" %in% itemNames) {
    newSeries$type = "line"
  }

  xAxis = attr(newSeries, "xAxis")
  yAxis = attr(newSeries, "yAxis")
  if (is.null(xAxis)) {
    # OVERLAP
  } else {
    # NEED GRID
  }

  if (is.null(yAxis)) {
    # OVERLAP
  } else {
    # MULIPLE Y-AXIS
    if ("type" %in% names(chart$x$yAxis)) {
      chart$x$yAxis = list(chart$x$yAxis, yAxis)
    } else {
      chart$x$yAxis = c(chart$x$yAxis, list(yAxis))
    }
    newSeries$yAxisIndex = length(chart$x$yAxis)-1
  }

  # MERGE SERIES
  if ("data" %in% names(chart$x$series)) {
    chart$x$series = list(chart$x$series, newSeries)
  } else {
    chart$x$series = c(chart$x$series, list(newSeries))
  }

  chart

}
