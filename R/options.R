#' Create an axis for a chart
#'
#' Add an axis to a chart.
#'
#' This function modified a few default options for the axis component in
#' ECharts: 1) \code{scale = TRUE} (was \code{FALSE} by default in ECharts); 2)
#' \code{axisLine$onZero = FALSE} (was \code{TRUE} in ECharts).
#' @export
#' @rdname axis
eAxis = function(
  chart, which = c('x', 'y'),
  type = c('value', 'category', 'time','log'), show = TRUE,
  position = c('bottom', 'top', 'left', 'right'),
  name = '', nameLocation = c('end', 'start'), nameTextStyle = emptyList(),
  boundaryGap = c(0, 0), min = NULL, max = NULL, scale = TRUE, splitNumber = NULL,
  axisLine = list(show = TRUE, onZero = FALSE), axisTick = list(show = FALSE),
  axisLabel = list(show = TRUE), splitLine = list(show = TRUE),
  splitArea = list(show = FALSE), data = list()
) {
  which = match.arg(which)
  odata = getMeta(chart)[[which]]  # original data along the axis
  if (missing(type)) type = axisType(odata, which)
  if (missing(position)) position = if (which == 'x') 'bottom' else 'left'
  if (missing(data) && type == 'category') {
    data = I(levels(as.factor(odata)))
  }

  x = chart$x
  i = paste0(which, 'Axis')
  o = list(
    type = match.arg(type), show = show, position = match.arg(position),
    name = name, nameLocation = match.arg(nameLocation), nameTextStyle = nameTextStyle,
    boundaryGap = boundaryGap, min = min, max = max, scale = scale,
    splitNumber = splitNumber, axisLine = axisLine, axisTick = axisTick,
    axisLabel = axisLabel, splitLine = splitLine, splitArea = splitArea, data = data
  )
  if (length(x[[i]])) {
    # only merge the arguments that are not missing, e.g. eAxis(min = 0) will
    # only override 'min' but will not override the 'name' attribute
    a = intersect(names(as.list(match.call()[-1])), names(o))#;browser()
    x[[i]] = mergeList(x[[i]], o[a])
  } else {
    x[[i]] = mergeList(x[[i]], o)
  }
  chart$x = x

  chart
}

#' @export
#' @rdname axis
eXAxis = function(chart, ...) {
  eAxis(chart, which = 'x', ...)
}


#' @export
#' @rdname axis
eYAxis = function(chart, ...) {
  eAxis(chart, which = 'y', ...)
}

axisType = function(data, which = c('x', 'y')) {
  if (is.numeric(data) || is.null(data)) return('value')
  if (is.factor(data) || is.character(data)) return('category')
  if (inherits(data, 'Date')) return('time')
  message('The structure of the ', which, ' variable:')
  str(data)
  stop('Unable to derive the axis type automatically from the ', which, ' variable')
}

axisTick = function(show=FALSE,
                    interval = c('auto',0),
                    onGap = NULL,
                    inside = FALSE,
                    length = 5,
                    lineStyleColor = '#333',
                    lineStyleWidth = 1) {
  return( list(
    show = show,
    interval = match.arg(interval),
    onGap    = onGap,
    inside   = inside,
    length   = length,
    lineStyle = list(color = lineStyleColor, width = lineStyleWidth)))
}






eThemeSet = function(chart, ...)
{
  themeArray <- c("macarons","infographic","shiny","dark","blue","green","red","gray","helianthus","roma","mint","macarons2","sakura","default")

  settings <- list(...)
  #print(settings)
  if (length(settings) == 0){
    return(chart)
  }

  usedSettings = c("theme")
  unusedSettings <- setdiff(names(settings), usedSettings)
  if (length(unusedSettings)>0){
    wm_1 <- paste("unused calculable setting inputs: ", paste(unusedSettings, collapse=","), ".", sep="")
    wm_2 <- paste("Acceptable calculable set: ", paste( usedSettings, collapse=", "), ".", sep="")
    warning(paste(wm_1, wm_2, sep="\n\r"))
  }

  # option$calculable format.
  if (!is.null(settings$theme)){
    if (is.numeric(settings$theme)){
      theme = themeArray[settings$theme]
      chart$x$theme = theme
    }else if(length(which(themeArray == settings$theme)) >0){
      chart$x$theme = settings$theme
    }else{
      wm_3 = paste("Invalid theme settings:", settings$theme)
      wm_4 = paste("You can type in the index or the name of theme Array: ",  paste( themeArray, collapse=","))
      warning(paste(wm_3, wm_4, sep="\n\r"))
      chart$x$theme = "default"
    }
  }
  chart
}

eTooltipSet = function(chart, ...)
{

  settings <- list(...)$optionList
  #print(settings)
  if (length(settings) == 0){
    return(chart)
  }

  usedSettings = c("show", "trigger", "formatter", "islandFormatter")
  unusedSettings <- setdiff(names(settings), usedSettings)
  if (length(unusedSettings)>0){
    wm_1 <- paste("unused calculable setting inputs: ", paste(unusedSettings, collapse=","), ".", sep="")
    wm_2 <- paste("Acceptable calculable set: ", paste( usedSettings, collapse=", "), ".", sep="")
    warning(paste(wm_1, wm_2, sep="\n\r"))
  }
  # option$tooltip format.
  if (!is.null(settings$show)){
    chart$x$tooltip$show = ifelse(settings$show, TRUE, FALSE)
  }
  if (!is.null(settings$trigger)){
    chart$x$tooltip$trigger = match.arg(settings$trigger, c("item","axis"))
  }
  if (!is.null(settings$formatter)){
    if(settings$formatter != ""){
      chart$x$tooltip$formatter = settings$formatter
    }else{
      chart$x$tooltip$formatter = NULL
    }
  }
  if (!is.null(settings$islandFormatter)){
    chart$x$tooltip$islandFormatter = settings$islandFormatter
  }
  chart

}

eLegendSet = function(chart, ...)
{
  settings <- list(...)$optionList
  #print(settings)
  if (length(settings) == 0){
    return(FALSE)
  }

  usedSettings = c("show", "data", "orient", "x", "y")
  unusedSettings <- setdiff(names(settings), usedSettings)
  if (length(unusedSettings)>0){
    wm_1 <- paste("unused calculable setting inputs: ", paste(unusedSettings, collapse=","), ".", sep="")
    wm_2 <- paste("Acceptable calculable set: ", paste( usedSettings, collapse=", "), ".", sep="")
    warning(paste(wm_1, wm_2, sep="\n\r"))
  }
  if (!is.null(settings$show)){
    chart$x$legend$show = ifelse(settings$show, TRUE, FALSE)
  }

  if(length(settings$data) == 0 & length(chart$x$legend$data)==0){
    chart$x$legend = list(show=FALSE)
  }else{
    #print(111)
    if(length(settings$data) > 0)chart$x$legend$data = as.list(settings$data)
  }

  if(chart$x$legend$show == TRUE){
    chart$x$legend$show = TRUE
    if (!is.null(settings$x)){
      chart$x$legend$x = matchPos.x(settings$x)
    }
    if (!is.null(settings$y)){
      chart$x$legend$y = matchPos.y(settings$y)
    }
    if (!is.null(settings$orient)){
      chart$x$legend$orient = match.arg(settings$orient, c("horizontal", "vertical"))
    }
  }else{
    chart$x$legend = list(show=FALSE)
    chart$x$legend$data = as.list(settings$data)
  }
  chart
}

eDataRangeSet = function(chart, ...)
{

  settings <- list(...)$optionList
  #print(settings)
  if (length(settings) == 0){
    return(FALSE)
  }

  usedSettings = c("show", "lim", "text", "orient", "x", "y",
                   "precision", "calculable", "color")
  unusedSettings <- setdiff(names(settings), usedSettings)
  if (length(unusedSettings)>0){
    wm_1 <- paste("unused calculable setting inputs: ", paste(unusedSettings, collapse=","), ".", sep="")
    wm_2 <- paste("Acceptable calculable set: ", paste( usedSettings, collapse=", "), ".", sep="")
    warning(paste(wm_1, wm_2, sep="\n\r"))
  }
  if (!is.null(settings$show)){
    chart$x$dataRange$show = ifelse(settings$show, TRUE, FALSE)
  }

  if (length(settings$lim) >= 2){
    chart$x$dataRange$min = floor(settings$lim[1]/10)*10
    chart$x$dataRange$max = floor(settings$lim[length(settings$lim)]/10)*10
  }

  if (length(settings$text) >= 2){
    chart$x$dataRange$text = settings$text[c(1, length(settings$text))]
    #chart$x$dataRange$min = floor(settings$lim[length(settings$lim)]/10)*10
  }
  if (!is.null(settings$orient)){
    chart$x$dataRange$orient = match.arg(settings$orient, c("horizontal", "vertical"))
  }
  if (!is.null(settings$x)){
    chart$x$dataRange$x = matchPos.x(settings$x)
  }
  if (!is.null(settings$y)){
    chart$x$dataRange$y = matchPos.y(settings$y)
  }
  if (!is.null(settings$y)){
    chart$x$dataRange$y = matchPos.y(settings$y)
  }

  if (!is.null(settings$calculable)){
    chart$x$dataRange$calculable = ifelse(settings$calculable, TRUE, FALSE)
  }
  if (length(settings$color) >= 2){
    chart$x$dataRange$color = settings$color[c(1, length(settings$color))]
  }

  chart
}


eAxis.XSet = function(chart, ...)
{

  settings <- list(...)$optionList
  if(length(settings) == 0){
    return(FALSE)
  }


  usedSettings = c("show", "type", "lim", "data", "scale", "position", "valueLabel",
                   "label.color", "label.fontSize", "label.margin", "label.rotate", "label.interval",
                   "tick.show", "tick.color", "tick.width", "tick.length",
                   "splitLine", "splitNumber", "splitArea",
                   "precision", "power", "gap", "name", "nameLocation")

  unusedSettings <- setdiff(names(settings), usedSettings)
  if (length(unusedSettings)>0){
    wm_1 <- paste("unused eAxis.X setting inputs: ", paste(unusedSettings, collapse=","), ".", sep="")
    wm_2 <- paste("Acceptable eAxis.X set: ", paste( usedSettings, collapse=", "), ".", sep="")
    warning(paste(wm_1, wm_2, sep="\n\r"))
  }

  if (!is.null(settings$show)){
    chart$x$xAxis$axisLabel$show = ifelse(settings$show, TRUE, FALSE)
  }

  if (!is.null(settings$type)){
    type = match.arg(settings$type, c("value", "category"))
    chart$x$xAxis$type = type
  }
  if (chart$x$xAxis$type == "value"){
    # if the axis type is value, the data argument is inactive
    if (length(settings$lim) >= 2){
      chart$x$xAxis$min = floor(settings$lim[1]/10)*10
      chart$x$xAxis$max = floor(settings$lim[length(settings$lim)]/10)*10
    }
    if(!is.null(settings$gap)){
      chart$x$xAxis$boundaryGap = ifelse(settings$gap, TRUE, FALSE)
    }

    if(!is.null(settings$scale)){
      chart$x$xAxis$scale = ifelse(settings$scale, TRUE, FALSE)
    }
  }else{

    # if the axis type is category, the lim argument is inactive
    if (length(settings$data) >= 1){
      chart$x$xAxis$data = settings$data
    }
    if(!is.null(settings$gap)){
      chart$x$xAxis$boundaryGap = ifelse(settings$gap, TRUE, FALSE)
    }
  }

  if (!is.null(settings$scale)){
    chart$x$xAxis$scale = ifelse(settings$scale, TRUE, FALSE)
  }

  if (!is.null(settings$position)){
    position = match.arg(settings$position, c("bottom", "top"))
    chart$x$xAxis$position = position
  }

  # xAxis Label settings.
  if (!is.null(settings$valueLabel)){
    chart$x$xAxis$axisLabel$formatter = paste("{value}", settings$valueLabel, sep="")
  }
  if(!is.null(settings$label.color)){
    chart$x$xAxis$axisLabel$textStyle$color = settings$label.color
  }
  if(!is.null(settings$label.fontSize)){
    chart$x$xAxis$axisLabel$textStyle$fontSize = as.numeric(settings$label.fontSize)
  }
  if(!is.null(settings$label.margin)){
    chart$x$xAxis$axisLabel$margin = as.numeric(settings$label.margin)
  }
  if(!is.null(settings$label.rotate)){
    chart$x$xAxis$axisLabel$rotate = as.numeric(settings$label.rotate)
  }
  if(!is.null(settings$label.interval)){
    if(tolower(settings$label.interval) != "auto" ){
      if(is.na(as.numeric(settings$label.interval))){
        settings$label.interval = "auto"
      }else{
        settings$label.interval = as.numeric(settings$label.interval)
      }
    }else{
      settings$label.interval = "auto"
    }
    chart$x$xAxis$axisLabel$interval = settings$label.interval
  }
  # xAxis tick settings.
  if(!is.null(settings$tick.show)){
    chart$x$xAxis$axisTick$show = ifelse(settings$tick.show, TRUE, FALSE)
  }
  if(!is.null(settings$tick.color)){
    chart$x$xAxis$axisTick$show = TRUE
    chart$x$xAxis$axisTick$lineStyle$color = settings$tick.color
  }
  if(!is.null(settings$tick.width)){
    chart$x$xAxis$axisTick$show = TRUE
    chart$x$xAxis$axisTick$lineStyle$width = as.numeric(settings$tick.width)
  }
  if(!is.null(settings$tick.length)){
    chart$x$xAxis$axisTick$show = TRUE
    chart$x$xAxis$axisTick$lineStyle$length = as.numeric(settings$tick.length)
  }

  #xAxis splitLine
  if(!is.null(settings$splitLine)){
    chart$x$xAxis$splitLine$show = ifelse(settings$splitLine, TRUE, FALSE)
  }

  #split Number only for value type.
  if(!is.null(settings$splitNumber)){
    chart$x$xAxis$splitNumber = as.numeric(settings$splitNumber)
  }

  if(!is.null(settings$splitArea)){
    chart$x$xAxis$splitArea = ifelse(settings$splitArea, TRUE, FALSE)
  }

  if(!is.null(settings$precision)){
    chart$x$xAxis$precision = as.integer(settings$precision)
  }

  if(!is.null(settings$power)){
    chart$x$xAxis$precision = as.integer(settings$power)
  }



  if(!is.null(settings$name)){
    chart$x$xAxis$name = settings$name
  }

  if(!is.null(settings$nameLocation)){
    chart$x$xAxis$nameLocation = match.arg(settings$nameLocation, c("start", "end"))
  }
  chart

}

eAxis.YSet = function(chart, ...)
{

  settings <- list(...)$optionList
  #print(settings)
  if(length(settings) == 0){
    return(FALSE)
  }

  usedSettings = c("show", "type", "lim", "data", "scale", "position", "valueLabel",
                   "label.color", "label.fontSize", "label.margin", "label.rotate", "label.interval",
                   "tick.show", "tick.color", "tick.width", "tick.length",
                   "splitLine", "splitNumber", "splitArea",
                   "precision", "power", "gap", "name", "nameLocation")

  unusedSettings <- setdiff(names(settings), usedSettings)
  if (length(unusedSettings)>0){
    wm_1 <- paste("unused eAxis.Y setting inputs: ", paste(unusedSettings, collapse=","), ".", sep="")
    wm_2 <- paste("Acceptable eAxis.Y set: ", paste( usedSettings, collapse=", "), ".", sep="")
    warning(paste(wm_1, wm_2, sep="\n\r"))
  }

  if (!is.null(settings$show)){
    chart$x$yAxis$axisLabel$show = ifelse(settings$show, TRUE, FALSE)
  }

  if (!is.null(settings$type)){
    type = match.arg(settings$type, c("value", "category"))
    chart$x$yAxis$type = type
  }
  if (chart$x$yAxis$type == "value"){
    # if the axis type is value, the data argument is inactive
    if (length(settings$lim) >= 2){
      chart$x$yAxis$min = floor(settings$lim[1]/10)*10
      chart$x$yAxis$max = floor(settings$lim[length(settings$lim)]/10)*10
    }
    if(!is.null(settings$gap)){
      if (length(settings$gap)!=2){
        warning("the gap argument only allow a numeric vector([0,0]) as input.")
      }else{
        chart$x$yAxis$boundaryGap = as.numeric(settings$gap)
      }
    }
    if(!is.null(settings$scale)){
      chart$x$yAxis$scale = ifelse(settings$scale, TRUE, FALSE)
    }
  }else{
    # if the axis type is category, the lim argument is inactive
    if (length(settings$data) >= 1){
      chart$x$yAxis$data = as.vector(settings$data)
    }
    if(!is.null(settings$gap)){
      chart$x$yAxis$boundaryGap = ifelse(settings$gap, TRUE, FALSE)
    }
  }

  if (!is.null(settings$scale)){
    chart$x$yAxis$scale = ifelse(settings$scale, TRUE, FALSE)
  }

  if (!is.null(settings$position)){
    position = match.arg(settings$position, c("left", "right"))
    chart$x$yAxis$position = position
  }

  # yAxis Label settings.
  if (!is.null(settings$valueLabel)){
    chart$x$yAxis$axisLabel$formatter = paste("{value}", settings$valueLabel, sep="")
  }
  if(!is.null(settings$label.color)){
    chart$x$yAxis$axisLabel$textStyle$color = settings$label.color
  }
  if(!is.null(settings$label.fontSize)){
    chart$x$yAxis$axisLabel$textStyle$fontSize = as.numeric(settings$label.fontSize)
  }
  if(!is.null(settings$label.margin)){
    chart$x$yAxis$axisLabel$margin = as.numeric(settings$label.margin)
  }
  if(!is.null(settings$label.rotate)){
    chart$x$yAxis$axisLabel$rotate = as.numeric(settings$label.rotate)
  }
  if(!is.null(settings$label.interval)){
    if(tolower(settings$label.interval) != "auto" ){
      if(is.na(as.numeric(settings$label.interval))){
        settings$label.interval = "auto"
      }else{
        settings$label.interval = as.numeric(settings$label.interval)
      }
    }else{
      settings$label.interval = "auto"
    }
    chart$x$yAxis$axisLabel$interval = settings$label.interval
  }
  # yAxis tick settings.
  if(!is.null(settings$tick.show)){
    chart$x$yAxis$axisTick$show = ifelse(settings$tick.show, TRUE, FALSE)
  }
  if(!is.null(settings$tick.color)){
    chart$x$yAxis$axisTick$show = TRUE
    chart$x$yAxis$axisTick$lineStyle$color = settings$tick.color
  }
  if(!is.null(settings$tick.width)){
    chart$x$yAxis$axisTick$show = TRUE
    chart$x$yAxis$axisTick$lineStyle$width = as.numeric(settings$tick.width)
  }
  if(!is.null(settings$tick.length)){
    chart$x$yAxis$axisTick$show = TRUE
    chart$x$yAxis$axisTick$lineStyle$length = as.numeric(settings$tick.length)
  }

  #yAxis splitLine
  if(!is.null(settings$splitLine)){
    chart$x$yAxis$splitLine$show = ifelse(settings$splitLine, TRUE, FALSE)
  }

  #split Number only for value type.
  if(!is.null(settings$splitNumber)){
    chart$x$yAxis$splitNumber = as.numeric(settings$splitNumber)
  }

  if(!is.null(settings$splitArea)){
    chart$x$yAxis$splitArea = ifelse(settings$splitArea, TRUE, FALSE)
  }

  if(!is.null(settings$precision)){
    chart$x$yAxis$precision = as.integer(settings$precision)
  }

  if(!is.null(settings$power)){
    chart$x$yAxis$precision = as.integer(settings$power)
  }

  if(!is.null(settings$name)){
    chart$x$yAxis$name = settings$name
  }

  if(!is.null(settings$nameLocation)){
    chart$x$yAxis$nameLocation = match.arg(settings$nameLocation, c("start", "end"))
  }
  chart

}


ePolarSet = function(chart, ...)
{

  settings <- list(...)$optionList
  #print(settings)
  if(length(settings) == 0){
    return(FALSE)
  }

  usedSettings = c("index", "center", "name", "lim", "radius", "scale", "startAngle", "splitNumber",
                   "precision", "power", "axisLine", "axisLabel", "splitLine", "splitArea")

  unusedSettings <- setdiff(names(settings), usedSettings)
  if (length(unusedSettings)>0){
    wm_1 <- paste("unused ePolar setting inputs: ", paste(unusedSettings, collapse=","), ".", sep="")
    wm_2 <- paste("Acceptable ePolar set: ", paste( usedSettings, collapse=", "), ".", sep="")
    warning(paste(wm_1, wm_2, sep="\n\r"))
  }

  ## name and lim setting.
  if(is.null(settings$index)){
    # set the polar[[index]] with name and lim.
    if(max(settings$index) > length(chart$x$polar[[1]]$indicator)){
      stop("index input has invalid elements.")
      # jump out loop
    }
    if( !is.null( settings$name)){
      if(length(settings$name) != length(settings$index)){
        stop("length of name argument is not equal to index argument.")
      }
      for(index in settings$index){
        chart$x$polar[[1]]$indicator[[index]]$text = settings$name[which(settings$index == index)]
      }
    }
    if( !is.null( settings$lim )){
      settings$lim = as.numeric(matrix(settings$lim, ncol=2))
      if(length(settings$lim[,1]) != length(settings$index)){
        stop("length of lim argument is not equal to index argument.")
      }
      for(index in settings$index){
        lim <- settings$lim[which(settings$index == index),]
        if (!is.na(lim[1])) chart$x$polar[[1]]$indicator[[index]]$min = lim[1]
        if (!is.na(lim[2])) chart$x$polar[[1]]$indicator[[index]]$max = lim[2]
      }
    }
  }else{
    # set the polar with name and lim object.
    if( !is.null( settings$name)){
      if(length(settings$name) != length(chart$x$polar[[1]]$indicator)){
        stop("length of name argument is not equal to the length of given data.")
      }
      for(index in seq(1,length(chart$x$polar[[1]]$indicator))){
        chart$x$polar[[1]]$indicator[[index]]$text = settings$name[index]
      }
    }

    if( !is.null( settings$lim )){
      settings$lim = as.numeric(matrix(settings$lim, ncol=2))
      if(length(settings$lim[,1]) != length(chart$x$polar[[1]]$indicator)){
        stop("length of lim argument is not equal to the length of given data.")
      }
      for(index in seq(1,length(chart$x$polar[[1]]$indicator))){
        lim <- settings$lim[index,]
        if (!is.na(lim[1])) chart$x$polar[[1]]$indicator[[index]]$min = lim[1]
        if (!is.na(lim[2])) chart$x$polar[[1]]$indicator[[index]]$max = lim[2]
      }
    }
  }

  #set center
  if(length(settings$center) > 0 & length(settings$center) != 2){
    warning("the center argment should be length of 2.")
  }else if(length(settings$center) == 2){
    chart$x$polar[[1]]$center = settings$center
  }

  #set radius
  if(length(settings$radius) > 0){
    chart$x$polar[[1]]$radius = as.numeric(settings$radius[[1]])
  }

  #set scale
  if(!is.null(settings$scale)){
    chart$x$polar[[1]]$scale = ifelse(settings$scale, TRUE, FALSE)
  }

  #set startAngle
  if(length(settings$startAngle) > 0){
    chart$x$polar[[1]]$radius = as.numeric(settings$radius[[1]])
  }

  #set splitNumber
  if(!is.null(settings$splitNumber)){
    chart$x$polar[[1]]$splitNumber = as.numeric(settings$splitNumber)
  }

  #set precision
  if(!is.null(settings$precision)){
    chart$x$polar[[1]]$precision = as.numeric(settings$precision)
  }
  #set power
  if(!is.null(settings$power)){
    chart$x$polar[[1]]$power = as.numeric(settings$power)
  }

  #set name formatter, "" will make the name null.
  if(!is.null(settings$formatter)){
    chart$x$polar[[1]]$name$formatter = formatter
  }
  #set axisLine show or not
  if(!is.null(settings$axisLine)){
    chart$x$polar[[1]]$axisLine$show = ifelse(settings$axisLine, TRUE, FALSE)
  }

  #set axisLabel show or not
  if(!is.null(settings$axisLabel)){
    chart$x$polar[[1]]$axisLabel$show = ifelse(settings$axisLabel, TRUE, FALSE)
  }

  #set splitLine show or not
  if(!is.null(settings$splitLine)){
    chart$x$polar[[1]]$splitLine$show = ifelse(settings$splitLine, TRUE, FALSE)
  }
  #set splitArea show or not
  if(!is.null(settings$splitArea)){
    chart$x$polar[[1]]$splitArea$show = ifelse(settings$splitArea, TRUE, FALSE)
  }

  chart
}


#' recharts set fucntion
#'
#' An shell function for setting arguments of the recharts object.
#'
#' @param obj  recharts object.
#' @return The output list of recharts as a list.
#' @export

optionSet <- function(chart, ...){

  settings <- list(...)$optionList
  #print(settings)
  if(length(settings) == 0){
    return(FALSE)
  }

  usedSettings = c("size", "calculable", "region")

  unusedSettings <- setdiff(names(settings), usedSettings)
  if (length(unusedSettings)>0){
    wm_1 <- paste("unused eOption setting inputs: ", paste(unusedSettings, collapse=","), ".", sep="")
    wm_2 <- paste("Acceptable eOption set: ", paste( usedSettings, collapse=", "), ".", sep="")
    warning(paste(wm_1, wm_2, sep="\n\r"))
  }

  if(length(settings$size) == 2){
    chart$x$size = as.numeric(settings$size)
  }

  if (!is.null(settings$calculable)){
    chart$x$calculable = ifelse(settings$calculable, TRUE, FALSE)
  }
  #print(settings$region)
  if (!is.null(settings$region)){
    newRegion = settings$region
    for (i in 1:length(chart$x$series)){
      chart$x$series[[i]]$mapType = newRegion
    }
  }
  chart = htmlwidgets::createWidget(
    'echarts', chart$x,
    package = 'recharts', width = chart$x$size[1], height = chart$x$size[2],
    preRenderHook = function(instance) {
      instance
    }
  )
  chart
}




legendSet = function(show=TRUE, data=NULL, orient = c("horizontal", "vertical"), legend.x="left", legend.y="top"){
  if(is.null(data)){show=FALSE}
  if(show){
    returnList <- list(
      show=TRUE,
      orient = match.arg(orient),
      x = matchPos.x(legend.x),
      y = matchPos.y(legend.y),
      data = as.list(data)
    )
    return(returnList)
  }else{
    return(list(
      show=FALSE,
      data = as.list(data)
    ))
  }
}

dataRangeSet = function(dataRange=TRUE, max=NULL, min=NULL, dataRange.text=c("high","low"), dataRange.x="left", dataRange.y="bottom",
                        precision=1, calculable=TRUE, color=c("#1e90ff", "#f0ffff"), orient=c("vertical", "horizontal"))
{

  if (length(color) >= 2){
    color = color[c(1, length(color))]
  }else{
    color = c("#1e90ff", "#f0ffff")
  }

  if (length(dataRange.text) >= 2){
    dataRange.text = dataRange.text[c(1, length(dataRange.text))]
  }else{
    dataRange.text = c("high","low")
  }

  returnList <- list(
    show = ifelse(dataRange, TRUE, FALSE),
    text = dataRange.text,
    x = matchPos.x( dataRange.x),
    y = matchPos.y( dataRange.y),
    calculable = ifelse(calculable, TRUE, FALSE),
    color = color,
    precision = precision,
    orient = match.arg(orient)
  )
  if (!is.null(max)){max=ceiling(max/10)*10}
  if (!is.null(min)){min=floor(min/10)*10}
  returnList$max = max
  returnList$min = min

  return(returnList)
}

xAxisSet = function(axisShow=TRUE, type=c("value", "category"), position=c("bottom", "top"),
                    labelName="", label.namePosition=c("start", "end"), scale=TRUE, lim=NULL,
                    axisLine=TRUE, axisTick=FALSE, axisLable=TRUE, splitLine=TRUE, splitArea=FALSE,
                    boundaryGap=TRUE, data=NULL, power=2, precision=2, color = "black")
{

  returnList <- list(
    position = match.arg(position),
    name = labelName,
    nameLocation = match.arg(label.namePosition),
    scale = ifelse(scale, TRUE, FALSE),
    precision = precision,
    power = power,
    axisLine = list(show = ifelse(axisLine, TRUE, FALSE)),
    axisTick = list(show = ifelse(axisTick, TRUE, FALSE)),
    axisLable = list(show = ifelse(axisLable, TRUE, FALSE), textStyle=list(color=color)),
    splitLine = list(show = ifelse(splitLine, TRUE, FALSE)),
    splitArea = list(show = ifelse(splitArea, TRUE, FALSE))
  )

  if(is.null(lim)){
    returnList$max = NULL
    returnList$min = NULL
  }else{
    returnList$max = max(as.numeric(lim))
    returnList$min = min(as.numeric(lim))
  }

  if (type == "value"){
    returnList$type = "value"
    if(is.logical(boundaryGap)){boundaryGap=c(0,0)}
    returnList$boundaryGap = boundaryGap
  }else{
    if(is.null(data)){
      data=""
      warning("no axis data for category axis.")
    }
    returnList$type = "category"
    returnList$data = data
    returnList$boundaryGap = ifelse(boundaryGap, TRUE, FALSE)

  }

  return(returnList)

}

yAxisSet = function(axisShow=TRUE, type=c("value", "category"), position=c("left", "right"),
                    labelName="", label.namePosition=c("start", "end"), scale=TRUE, lim=NULL,
                    axisLine=TRUE, axisTick=FALSE, axisLable=TRUE, splitLine=TRUE, splitArea=FALSE,
                    boundaryGap=TRUE, data=NULL, power=2, precision=2)
{

  type = match.arg(type)

  returnList <- list(
    position = match.arg(position),
    name = labelName,
    nameLocation = match.arg(label.namePosition),
    scale = ifelse(scale, TRUE, FALSE),
    precision = precision,
    power = power,
    axisLine = list(show = ifelse(axisLine, TRUE, FALSE)),
    axisTick = list(show = ifelse(axisTick, TRUE, FALSE)),
    axisLable = list(show = ifelse(axisLable, TRUE, FALSE)),
    splitLine = list(show = ifelse(splitLine, TRUE, FALSE)),
    splitArea = list(show = ifelse(splitArea, TRUE, FALSE))
  )

  if(is.null(lim)){
    returnList$max = NULL
    returnList$min = NULL
  }else{
    returnList$max = max(as.numeric(lim))
    returnList$min = min(as.numeric(lim))
  }

  if (type == "value"){
    returnList$type = "value"
    if(is.logical(boundaryGap)){boundaryGap=c(0,0)}
    returnList$boundaryGap = boundaryGap
  }else{
    if(is.null(data)){
      data=""
      warning("no axis data for category axis.")
    }
    returnList$type = "category"
    returnList$data = data
    returnList$boundaryGap = ifelse(boundaryGap, TRUE, FALSE)

  }
  return(returnList)

}

polarSet = function(name=NULL, ymin=NULL, ymax=NULL, center=NULL, radius=NULL, startAngle=90, splitNumber=NULL,
                    scale=FALSE, precision=0, power=100, axisLine=TRUE, axisLabel=FALSE,
                    splitLine=TRUE, splitArea=TRUE)
{
  indicator <- vector("list", length(name))

  for(i in 1:length(name)){
    indicator[[i]]$text = name[i]

    if(!is.null(ymin[i]) & !is.na(ymin[i])){
      indicator[[i]]$min = as.numeric(ymin[i])
    }

    if(!is.null(ymax[i]) & !is.na(ymax[i])){
      indicator[[i]]$max = as.numeric(ymax[i])
    }
  }

  indicatorList <- indicator
  #print(indicatorList)
  returnList <- list(
    indicator = indicator,
    startAngle = startAngle,
    scale = scale,
    precision = precision,
    power = power,
    axisLine = list(show = axisLine),
    axisLabel = list(show = axisLabel),
    splitLine = list(show = splitLine),
    splitArea = list(show = splitArea)
  )

  returnList$center <- center
  returnList$radius <- radius
  returnList$splitNumber <- splitNumber

  return(returnList)
}
