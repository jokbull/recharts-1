

defaultSetting_common = function(chart, ...) {

  if ("type" %in% names(chart$x$xAxis)) {
    seriesNumber = 1
  } else {
    seriesNumber = length(chart$x$xAxis)
  }
  if ("data" %in% names(chart$x$series)) {
    legendName = NULL
  } else {
    legendName = sapply(chart$x$series,function(x) x$name)
  }


  newop = list(
    legend = list(
      data = legendName
    ),

    toolbox = list(
      feature=list(
        saveAsImage=list(),
        restore=list(),
        dataView=list(),
        dataZoom=list(),
        magicType=list(),
        brush=list()
      )
    ),
    tooltip = list(
      show = TRUE
    ),
    dataZoom = list(
      list(
        type = "slider",
        show = TRUE
      ),
      list(
        type = "inside"
      )
      # list(
      #   type = "slider",
      #   show=TRUE,
      #   yAxisIndex = 0
      # ),
      # list(
      #   type = "inside",
      #   yAxisIndex = 0
      # )
    )
    # xAxis = list(
    #   splitLine = list(show=TRUE)
    # )
    # yAxis = list(
    #   splitLine = list(show=TRUE)
    # )

  )

  if (seriesNumber>1) {
    newop$dataZoom[[1]]$xAxisIndex = seq_len(seriesNumber) - 1
  }
  setOptions(chart,newop)

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




yAxisStr = "yAxis: [
            {
                scale: true,
                splitArea: {
                    show: true
                }
            },
            {
                scale: true,
                gridIndex: 1,
                splitNumber: 2,
                axisLabel: {show: false},
                axisLine: {show: false},
                axisTick: {show: false},
                splitLine: {show: false}
            }
        ]"
