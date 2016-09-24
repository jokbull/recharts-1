
param_line = function(dat, x, y, series, ...) {
  params = list()
  # fetch data
  xvar = evalFormula(x, dat)
  yvar = evalFormula(y, dat)
  seriesData = evalFormula(series, dat)

  # these three are only names.
  xlabName = autoArgLabel(x, deparse(substitute(x)))
  ylabName = autoArgLabel(y, deparse(substitute(y)))
  seriesName = autoArgLabel(series, deparse(substitute(series)))

  if(is.null(xvar) & is.null(yvar) & !is.factor(dat)){
    # Mode 1. use default data.frame as input...
    plotData <- as.data.frame(dat, stringsAsFactor=F)
  }else if(!is.null(xvar) & !is.null(yvar) & !is.null(series)){
    # Mode 2. all of xvar, yvar and series are valid...
    xvarArray = unique(as.character(xvar))
    seriesArray = unique(as.character(series))
    dataMatrix = xtabs(as.formula(paste0(ylabName, "~", xlabName , "+",  seriesName)), dat)
    plotData <- as.data.frame.matrix(dataMatrix[xvarArray,seriesArray])
  }else if(!is.null(xvar) & !is.null(yvar) & is.null(series)){
    # Mode 3. format dat with only x and y variable.
    plotData <- data.frame(val = yvar)
    colnames(plotData) <- ylabName
    rownames(plotData) <- xvar
  }else if(is.null(xvar) & is.null(yvar) & is.factor(dat)){
    # Mode 4. factor
    tempD <- as.data.frame(table(dat))
    plotData <- data.frame(val = tempD[,"Freq"])
    colnames(plotData) <- "Frequency"
    rownames(plotData) <- tempD[,1]
  }


  xAxis = list()
  yAxis = list()
  if (is.numeric(xvar)) {
    xAxis$data = xvar
  } else {
    xAxis$data = rownames(plotData)
  }
  xAxis$type = axisType(xAxis$data,'x')

  if (is.numeric(yvar)) {
    yAxis$data = yvar
  } else {
    yAxis$data = colnames(plotData)
  }
  yAxis$type = axisType(yAxis$data,'y')

  optseries = vector("list", ncol(plotData))

  for(i in 1:ncol(plotData)) {
    if(is.null(optseries[[i]]$type)) {
      optseries[[i]]$type = 'line'
    }
    if(is.null(optseries[[i]]$name)) {
      optseries[[i]]$name = colnames(plotData)[i]
    } else {
      warning('You can set series:name with colnames(dat).')
    }

    if(is.null(optseries[[i]]$data)) {
      optseries[[i]]$data = unnames(plotData[,i])
    } else {
      warning('You can set series:data with dat.')
    }
  }




  structure(list(
    series = optseries,
    xAxis = xAxis, yAxis = yAxis
  ), meta = list(
    x = xAxis$data, y = yAxis$data
  ))

}


param_k = function(dat, x, y, series, ...) {
  params = list()
  settings = list(...)
  # check
  stopifnot(hasArg("open"))
  stopifnot(hasArg("high"))
  stopifnot(hasArg("low"))
  stopifnot(is.null(series))

  # fetch data
  if (hasArg("volume")) {
    volume = evalFormula(settings$volume,dat)
  } else {
    volume = NULL
  }

  open = evalFormula(settings$open, dat)
  high = evalFormula(settings$high, dat)
  low  = evalFormula(settings$low, dat)
  if (hasArg("close")) {
    close= evalFormula(settings$close, dat)
  } else {
    close= evalFormula(y, dat)
  }
  xvar = evalFormula(x, dat)
  plotData = cbind(open,close,low,high)

  optseries = list(type="candlestick","data" = plotData)
  xAxis = list(type = "category", "data" = xvar)
  yAxis = list(type = "value", scale=TRUE)

  if (!is.null(volume)) {
    xAxis2 = xAxis
    xAxis2$gridIndex = 1


    xAxis = list(xAxis, xAxis2)
    yAxis2 = yAxis
    yAxis2 = rlist::list.merge(yAxis2, list(
      gridIndex=1,
      axisLabel=list(show=FALSE),
      axisLine=list(show=FALSE),
      axisTick=list(show=FALSE),
      splitLine=list(show=FALSE)
    ))
    yAxis = list(yAxis,yAxis2)
    optseries = list(optseries, list(
      type="bar",
      name="Volume",
      xAxisIndex=1,
      yAxisIndex=1,
      data = volume
    ))
  }


  res = structure(list(
    series = optseries,
    xAxis = xAxis, yAxis = yAxis
  ), meta = list(
    x = xvar, y = plotData
  ))
  if (!is.null(volume)) {
    res[["grid"]] = list(list(left="10%",right="8%",height="50%"),list(left="10%",right="8%",top="63%",height="16%"))
  }
  res
}
