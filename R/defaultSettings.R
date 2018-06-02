

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
