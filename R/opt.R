#' @title set options in JSON format
#' @name AddItemFromJson
#' @param optionType = c("title","legend","grid","xAxis","yAxis","polar","radiusAxis","angleAxis","radar",
#'          "dataZoom", "visualMap", "toolbox","tooltip","brush","geo", "parallel", "parallelAxis",
#'          "singleAxis","timeline","textStyle","series","color","backgroundColor",
#'          "animation","animationThreshold","animationDuration","animationEasing","animationDelay",
#'          "animationDurationUpdate","animationEasingUpdate","animationDelayUpdate",
#'          "progressive","progressiveThreshold","blendMode","hoverLayerThreshold")
#' @export
setOptionsFromJson = function(chart, jsonStr
) {

  chart$x = rlist::list.merge(chart$x, fromJSON(regJson(jsonStr),simplifyVector = FALSE))
  chart
}
