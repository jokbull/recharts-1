#' Create the k chart
#' @export
eK = function(...) {
  echart(...,type="k")
}



defaultSetting_k = function(chart, ...) {
  newop = list(
    legendHoverLink = TRUE,
    tooltip = list(
      trigger="axis"
    )
    # markPoint = list(
    #   symbol = "triangle",
    #   symbolSize = 30,
    #   data = list(
    #     list(type="min",itemStyle=list(normal=list(color="#314656"))),
    #     list(type="max",symbolRotate=180)
    #   )
    # ),
    # markLine = list(
    #   data = list(
    #     list(
    #       list(type="min",symbol = 'none'),
    #       list(type="max")
    #     ),
    #     list(
    #       list(type="average",symbol = 'none'),
    #       list(type="max")
    #     )
    #   )
    # )
  )
  chart = defaultSetting_common(chart)
  if ("data" %in% names(chart$x$series) ) {
    chart$x$title = list(text=chart$x$series$name)
    chart$x$series = rlist::list.merge(chart$x$series,newop)
    # chart$x$tooltip = rlist::list.merge(chart$x$tooltip,
    #       list(formatter = JS(
    #         "function (param) {
    #         var param = param[0];
    #         return [
    #           'Date: ' + param.name + '<hr size=1 style=\"margin: 3px 0\">',
    #           'Open: ' + param.data[0] + '<br/>',
    #           'Close: ' + param.data[1] + '<br/>',
    #           'Lowest: ' + param.data[2] + '<br/>',
    #           'Highest: ' + param.data[3] + '<br/>'
    #
    #           ].join('');
    #       }"
    #
    # )))

  } else {
    chart$x$title = list(text=chart$x$series[[1]]$name)
    chart$x$series[[1]] = rlist::list.merge(chart$x$series[[1]],newop)
    #   chart$x$tooltip = rlist::list.merge(chart$x$tooltip,
    #                                       list(formatter = JS(
    #                                         "function (param) {
    #                                         var param = param[0];
    #                                         return [
    #                                         'Date: ' + param.name + '<hr size=1 style=\"margin: 3px 0\">',
    #                                         'Open: ' + param.data[0] + '<br/>',
    #                                         'Close: ' + param.data[1] + '<br/>',
    #                                         'Lowest: ' + param.data[2] + '<br/>',
    #                                         'Highest: ' + param.data[3] + '<br/>',
    #                                         'Volume: ' + param.data[4] + '<br/>'
    #                                         ].join('');
    # }"
    #
    #                                       )))
  }


  chart
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

