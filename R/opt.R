#' @title set options for echart
#' @name setOptions
#' @description List or JsonString
#' optionType = c("title","legend","grid","xAxis","yAxis","polar","radiusAxis","angleAxis","radar",
#'          "dataZoom", "visualMap", "toolbox","tooltip","brush","geo", "parallel", "parallelAxis",
#'          "singleAxis","timeline","textStyle","series","color","backgroundColor",
#'          "animation","animationThreshold","animationDuration","animationEasing","animationDelay",
#'          "animationDurationUpdate","animationEasingUpdate","animationDelayUpdate",
#'          "progressive","progressiveThreshold","blendMode","hoverLayerThreshold")
#' @export
setOptions = function(chart, x) {
  if (is.list(x)) {
    chart = .setOptionsFromList(chart,x)
  } else if (is.character(x) & jsonlite::validate(regJson(x))) {
    chart = .setOptionsFromJson(chart,x)
  } else {
    warning("Unknow the options in which format")
  }
  chart
}

.setOptionsFromList = function(chart, lst) {
  chart$x = rlist::list.merge(chart$x, lst)
  chart
}

.setOptionsFromJson = function(chart, jsonStr) {
  .setOptionsFromList(chart, fromJSON(regJson(jsonStr),simplifyVector = FALSE))
}


