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
  
#' Create a tooltip for a chart
#' 
#' Add the tooltip,(mouse hover show the details)
#' 
#' @param trigger item(default) or axis
#' @export
tooltip = function(
  chart,
  show = TRUE,
  showContent = TRUE,
  trigger = c('item','axis'),
  position = NULL,
  showDelay = 20,
  hideDelay = 100,
  transitionDuration = 0.4,
  enterable = FALSE,
  backgroundColor = 'rgba(0,0,0,0,7)',
  borderColor = '#333',
  borderRadius = 4,
  borderWidth = 0,
  padding = 5,
  #axisPointer = list(type='line',)
  textStyleColor = '#fff') {
  
  
  o = list(show = show,
           showContent = showContent,
           trigger = match.arg(trigger),
           position = position
           #showDelay = showDelay,
           #hideDelay = hideDelay,
           #transitionDuration = transitionDuration,
           #enterable = enterable,
           #backgroundColor = backgroundColor,
           #borderColor = borderColor,
           #borderWidth = borderWidth,
           #padding = padding,
           #textStyle = list(color=textStyleColor)
  )
  if ('tooltip' %in% names(chart$x)) {
    chart$x$tooltip = mergeList(chart$x$tooltip, o )
  } else {
    chart$x$tooltip = o
  }
  chart
}
  

#' Create a dataZoom widget for a chart
#' 
#' Add the dataZoom widget
#' @param zoomLock if lock the data window width.
#' @export
dataZoom = function(chart,
            show = TRUE,
            orient = c('horizontal', 'vertical'),
            start = 0,
            end = 100,
            showDetail = TRUE,
            realtime = TRUE,
            zoomLock = FALSE) {
  o = list(show = show, orient = match.arg(orient), start = start, end = end, showDetail = showDetail, realtime = realtime, zoomLock = zoomLock)
  if ('dataZoom' %in% names(chart$x)){
    chart$x$dataZoom = mergeList(chart$x$dataZoom, o)
  } else {
    chart$x$dataZoom = o
  }
  chart
}

#' Create the toolbox for a chart
#' 
#' Add toolbox with SaveAsImage button
#' @export
toolbox = function(chart) {
  chart$x$toolbox = list(show = TRUE, feature = list(saveAsImage = list(show=TRUE)))
  chart
}


#' Create a title/subtitle for a chart
#' 
#' Add a title/subtitle to a chart,
#' support Chinese character under linux, don't sure about windows.
#' 
#' @param text Title
#' @param link Title hyperlink
#' @param subtext Subtitle
#' @param sublink Subtitle hyperlink
#' @export
title = function(chart, show=TRUE,
                 text = '', link = '', subtext= '', sublink = '',
                 x = c('center', 'left', 'right'),
                 y = c('top', 'bottom', 'center'),
                 textStyle.fontSize = 12,
                 textStyle.fontStyle = c('normal','italic','oblique'),
                 textStyle.fontWeight = c('normal', 'bold', 'bolder', 'lighter')) {
  chart$x$title = list(show=TRUE,
                       text = text, link = link, subtext = subtext, sublink = sublink,
                       x = match.arg(x),
                       y = match.arg(y),
                       textStyle = list(fontSize = textStyle.fontSize, 
                                        fontStyle = match.arg(textStyle.fontStyle),
                                        fontWeight = match.arg(textStyle.fontWeight)))
  chart
}

  