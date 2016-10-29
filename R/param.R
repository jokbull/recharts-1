
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

  if (is.null(xvar) & is.null(yvar) & !is.factor(dat)) {
    # Mode 1. use default data.frame as input...
    plotData <- as.data.frame(dat, stringsAsFactor = F)
    warning("xvar/yvar are both null")
    return(NULL)
  } else if (!is.null(xvar) & !is.null(yvar) & !is.null(seriesData)) {
    # Mode 2. all of xvar, yvar and series are valid...
    xvar = unique(as.character(xvar))
    seriesArray = unique(as.character(seriesData))
    dataMatrix = xtabs(as.formula(paste0(ylabName, "~", xlabName , "+",  seriesName)), data = dat, na.action = na.pass)
    plotData = as.data.frame.matrix(dataMatrix[xvar,seriesArray])
  } else if (!is.null(xvar) & !is.null(yvar) & is.null(seriesData)) {
    # Mode 3. format dat with only x and y variable.
    plotData <- data.frame(val = yvar)
    colnames(plotData) <- ylabName
  } else if (!is.null(xvar) & is.null(yvar)) {

    # Mode 4. Density line.
    if (is.numeric(xvar)) {
      # continue x-axis to breaks
      if (is.null(series)) {
        tmp = hist(xvar, plot = F,...)
        xvar = as.character(tmp$mids)
        plotData = cbind(NULL,tmp$counts)
        colnames(plotData) = "Frequency"
      } else {
        univ = hist(xvar, plot = F, ...)
        univ_breaks = univ$breaks
        univ_seriesData = names(table(seriesData))
        plotData = NULL
        datasets = split(xvar, univ_seriesData)
        for (i in seq_along(datasets)) {
          pd = hist(dataset[i],breaks = univ_breaks)$counts
          plotData = cbind(plotData, pd)
        }
        colnames(plotData) = univ_seriesData
        xvar = as.character(univ$mids)
      }

    } else if (is.null(series)) {
      plotData = as.vector(unname(table(xvar)))
      xvar = list(type='category', data = names(plotData))
    } else {
      univ = names(table(xvar))
      univ_seriesData = names(table(seriesData))
      plotData = NULL
      datasets = split(xvar, factor(seriesData,univ_seriesData) )
      for (i in seq_along(datasets)) {
        pd = table(factor(datasets[[i]],univ))
        plotData = cbind(plotData, as.vector(unname(pd)))
      }
      xvar = univ
    }


  # } else if (is.null(xvar) & is.null(yvar) & is.factor(dat)) {
  #   # Mode 4. factor
  #   tempD <- as.data.frame(table(dat))
  #   plotData <- data.frame(val = tempD[,"Freq"])
  #   colnames(plotData) <- "Frequency"
  #   rownames(plotData) <- tempD[,1]
  }


  xAxis = list(
    data = xvar,
    type = axisType(xvar,'x')
  )
  yAxis = list(
    data = yvar,
    type = axisType(yvar,'y')
  )

  optseries = vector("list", ncol(plotData))

  for (i in 1:ncol(plotData)) {
    if (is.null(optseries[[i]]$type)) {
      optseries[[i]]$type = 'line'
    }
    if (is.null(optseries[[i]]$name)) {
      optseries[[i]]$name = colnames(plotData)[i]
    } else {
      warning('You can set series:name with colnames(dat).')
    }

    if (is.null(optseries[[i]]$data)) {
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
  seriesName = attr(dat,"symbol")
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
    close = evalFormula(settings$close, dat)
  } else {
    close = evalFormula(y, dat)
  }
  xvar = evalFormula(x, dat)
  plotData = cbind(open,close,low,high)

  optseries = list(type = "candlestick","data" = plotData,name = seriesName)
  xAxis = list(type = "category", "data" = xvar)
  yAxis = list(type = "value", scale = TRUE)

  if (!is.null(volume)) {
    xAxis2 = xAxis
    xAxis2$gridIndex = 1


    xAxis = list(xAxis, xAxis2)
    yAxis2 = yAxis
    yAxis2 = rlist::list.merge(yAxis2, list(
      gridIndex = 1,
      axisLabel = list(show = FALSE),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      splitLine = list(show = FALSE)
    ))
    yAxis = list(yAxis,yAxis2)
    optseries = list(optseries, list(
      type = "bar",
      name = "Volume",
      xAxisIndex = 1,
      yAxisIndex = 1,
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
    res[["grid"]] = list(list(left = "10%",right = "8%",height = "50%"),list(left = "10%",right = "8%",top = "63%",height = "16%"))
  }
  res
}



param_scatter = function(dat, x, y, series, coordinateSystem = "cartesian2d",...) {

  # if (effectScatter){
  #   type = "effectScatter"
  # } else {
  #   type = "scatter"
  # }

  if (hasArg(symbolSize)) {
    symbolSize = list(...)$symbolSize
  } else {
    symbolSize = 10
  }
  type = "scatter"
  params = list()
  # fetch data
  xvar = evalFormula(x, dat)
  yvar = evalFormula(y, dat)
  seriesData = evalFormula(series, dat)

  # these three are only names.
  xlabName = autoArgLabel(x, deparse(substitute(x)))
  ylabName = autoArgLabel(y, deparse(substitute(y)))
  seriesName = autoArgLabel(series, deparse(substitute(series)))




  optseries = list()
  if (is.null(series)) {
    optseries = list(
      data = cbind(xvar,yvar),
      type = type,
      large = TRUE,
      coordinateSystem=coordinateSystem)
  } else {
    plotData = split(as.data.frame(cbind(xvar,yvar)), seriesData)
    for (i in seq_along(plotData)) {
      optseries[[i]] = list(
        data = as.matrix(plotData[[i]]),
        name = names(plotData)[i],
        large = TRUE,
        type = type,
        coordinateSystem = coordinateSystem,
        symbolSize = symbolSize
      )
    }

  }

  xAxis = list()
  yAxis = list()
  xAxis$type = axisType(xAxis$data,'x')
  yAxis$type = axisType(yAxis$data,'y')


  structure(list(
    series = optseries,
    xAxis = xAxis, yAxis = yAxis
  ), meta = list(
    x = xAxis$data, y = yAxis$data
  ))

}






param_bar = function(dat, x, y, series,...){
  type = "bar"
  params = list()
  # fetch data
  xvar = evalFormula(x, dat)
  yvar = evalFormula(y, dat)
  seriesData = evalFormula(series, dat)

  # these three are only names.
  xlabName = autoArgLabel(x, deparse(substitute(x)))
  ylabName = autoArgLabel(y, deparse(substitute(y)))
  seriesName = autoArgLabel(series, deparse(substitute(series)))


  if (is.null(y)) {
    # plot the frequencies of x when y is not provided
    if (is.numeric(xvar)) {
      if (is.null(series)) {
        tmp = hist(xvar, plot = F,...)
        xvar = as.character(tmp$mids)

        optseries = list(name = "Frequency", data = tmp$counts, type = type)
        xAxis = list(type='category', data = xvar)
        yAxis = list(type="value")
      } else {
        univ = hist(xvar, plot = F, ...)
        univ_breaks = univ$breaks
        univ_seriesData = names(table(seriesData))
        xvar = as.character(univ$mids)
        optseries = vector(length(univ_seriesData),"list")
        xAxis = list(type='category', data = xvar)
        yAxis = list(type="value")
        datasets = split(xvar, univ_seriesData)
        for (i in seq_along(datasets)) {
          pd = hist(dataset[i],breaks = univ_breaks)$counts
          optseries[[i]] = list(name = univ_seriesData[i],
                                data = pd, type = type)
        }
      }
    } else if (is.null(series)) {
      plotData = table(xvar)
      optseries = list(name = ylabName, data = as.vector(unname(plotData)), type = type)
      xAxis = list(type='category', data = names(plotData))
      xAxis$type = axisType(xAxis$data,'x')
      yAxis = list(type="value")
    } else {
      univ = names(table(xvar))
      univ_seriesData = names(table(seriesData))
      optseries=list()
      plotData = split(as.data.frame(x=xvar), factor(seriesData,univ_seriesData) )
      for (i in seq_along(plotData)) {
        pd = table(factor(plotData[[i]][,1],univ))
        optseries[[i]] = list(
          data = as.vector(unname(pd)),
          name = univ_seriesData[i],
          type = type
        )
      }
      xAxis = list(type='category', data = names(table(xvar)))
      xAxis$type = axisType(xAxis$data,'x')
      yAxis = list(type="value")
    }
  }
  # when y is provided, use y as the height of bars
  else if (is.null(series)) {
    optseries = list(name = ylabName, data = yvar, type = type)
    xAxis = list(type='category', data = xvar)
    xAxis$type = axisType(xAxis$data,'x')
    yAxis = list(type="value")
    # return(list(list(type = type, data = y)))
  } else {
    # NEED make the every split Y length is the same
    # merge X first, then join Y.

    warning("Probabily this part has bug. when the X range is different among series")

    optseries=list()
    univ = names(table(xvar))
    univ_seriesData = names(table(seriesData))


    plotData = split(as.data.frame(x=xvar,y=yvar), factor(seriesData,univ_seriesData))
    for (i in seq_along(plotData)) {
      optseries[[i]] = list(
        data = as.vector(plotData[[i]][,2]),
        name = univ_seriesData[i],
        type = type
      )
    }
    xAxis = list(type='category', data = names(table(xvar)))
    xAxis$type = axisType(xAxis$data,'x')
    yAxis = list(type="value")
  }
  structure(list(
    series = optseries,
    xAxis = xAxis, yAxis = yAxis
  ), meta = list(
    x = xAxis$data, y = yAxis$data
  ))
}




