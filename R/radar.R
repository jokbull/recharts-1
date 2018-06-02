

#' Create the bar chart
#' @export
eRadar = function(...) {
  echart(...,type="radar")
}


defaultSetting_radar= function(chart, ...) {
  setOptions(chart, list("animation" = FALSE))
}


#' @example
#'
#' data.frame(item, value, max)
#'
#'

param_radar = function(dat, x, y, series, ...){
  type = "radar"
  params = list()
  # fetch data
  xvar = evalFormula(x, dat) # xvar is item
  yvar = evalFormula(y, dat) # yvar is value
  seriesData = evalFormula(series, dat)

  # these three are only names.
  xlabName = autoArgLabel(x, deparse(substitute(x)))
  ylabName = autoArgLabel(y, deparse(substitute(y)))
  seriesName = autoArgLabel(series, deparse(substitute(series)))


  # ...
  dots = list(...)
  if (hasArg("shape")) {
    shape = dots[["shape"]]
  } else {
    shape = "circle"
  }

  indicator = list()
  plotData = dcast(dat, formula(sprintf("%s ~ %s", seriesName, xlabName)), value.var = ylabName)
  plotData[seriesName] = NULL

  i = 0
  for (l in levels(xvar)) {
    i = i + 1
    indicator[[i]] = list(name = l, max = max(yvar[xvar == l]))
  }

  radarAxis = list(
    indicator = indicator,
    shape = shape,
    splitNumber = 5
  )


  optseries = list(
    data = as.matrix(plotData),
    type = type
  )

  structure(list(
    series = optseries,
    radar = radarAxis
  ), meta = list(
    x = radarAxis$data
  ))

}




