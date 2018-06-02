

#' Create the line chart
#' @export
eLine = function(...) {
  echart(...,type="line")
}

defaultSetting_line = function(chart, ...) {
  # plotData = getMeta(chart)
  # xvar = plotData$x
  # yvar = plotData$y
  setOptions(defaultSetting_common(chart),list(
    tooltip = list(
      trigger = "axis"
    )
  ))
}

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
