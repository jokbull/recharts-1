

#' Create the bar chart
#' @export
eBar = function(...) {
  echart(...,type="bar")
}


defaultSetting_bar= function(chart, ...) {
  setOptions(defaultSetting_common(chart),list(
    tooltip = list(
      trigger = "axis"
    ),
    toolbox = list(
      feature = list(
        magicType = list(
          type = c("stack","tiled")
        ),
        dataView = list(),
        saveAsImage = list()
      )
    )
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




