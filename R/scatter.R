#' Create the scatter chart
#' @export
eScatter = function(...) {
  echart(...,type="scatter")
}

defaultSetting_scatter= function(chart, ...) {
  # plotData = getMeta(chart)
  # xvar = plotData$x
  # yvar = plotData$y
  setOptions(defaultSetting_common(chart),list(
    tooltip = list(
      trigger = "axis"
    )
  ))
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




