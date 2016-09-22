#' Create an HTML line charts widget using the ECharts(version:3.2.2) library
#'
#' This function creates an HTML widget to display matrix, data.frame and
#' factor array, using the JavaScript library ECharts3.
#' @param dat a data object (a matrix, a data frame or a factor array)
#' @param xvar,yvar objects of class "formula" (or one that can be coerced
#'   to that class):  x,y coordinates of the given data.frame colnames, e.g.
#'   \code{xvar = ~xAxisName}; \code{yvar = ~yAxisName}. xvar, yvar only needed for the
#'   data.frame data input.
#' @param series an "formula" object: Associates the levels of variable
#'   with symbol color, e.g. \code{series = ~groupName}
#' @param size an array of html widget width and height(either numeric pixels
#'   or percentage could be accepted): e.g. size = c(1024, 768).
#' @param theme an object of theme name. see(\url{http://datatables.net/extensions/index}) for detail.
#'   supported theme: \code{c("default", "vintage", "dark", "westeros", "essos", "wonderland", "walden",
#'   "chalk", "infographic", "macarons", "roma", "shine", "purple-passion")}
#' @param title an overall title for the plot. you can modify title widget after chart has been
#'   generated: Echart + eTitle(title = "your title.")
#' @param title.x,title.y the xy coordinates of main title, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify title widget after chart has been generated:
#'   \code{Echart + eTitle(title="main title", x = "left", y=10)}
#' @param legend logical whether the legend widget show or not, default is TRUE.
#'   you can modify legend widget after chart has been generated, the legend position and
#'   legend orientation are available at present.
#'   \code{Echart + eLegend(show = TRUE)}
#' @param legend.x,legend.y the xy coordinates of legend, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(x = "right", y="top")}
#' @param legend.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(orient = "vertical")}
#' @param toolbox logical whether the toolbox widget show or not, default is TRUE.
#'   you can modify toolbox widget after cahart has been generated, the toolbox position, toolbox
#'   element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(show = TRUE)}
#' @param toolbox.x,toolbox.y the xy coordinates of toolbox, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(x = "right", y="top")}
#' @param toolbox.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(orient = "vertical")}
#' @param dataview,mark,restore,dataZoom,magicType logical variable whether the dataview
#'   , mark, restore, dataZoom or magicType tool in toolbox widget show or not,
#'   default is TRUE. you can modify toolbox widget after chart has been generated,
#'   the toolbox position, toolbox element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(dataView = FALSE)}
#' @param tooltip logical whether the tooltip widget for front-end interactive chart
#'   show or not. default is TRUE. you can modify tooltip widget after chart has been generated,
#'   the tooltip trigger and tooltip formatter is available at present.
#'   \code{Echart + eTooltip(show = FALSE)}
#' @param tooltip.trigger an element of c("axis", "item"), default is "axis" for line chart.
#'   "axis" option for trigger will show all the information of mouse;
#'   "item" option for tirgger will only show the given item information of mouse.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(trigger = "axis")}
#' @param tooltip.formatter the information formatter for tooltip widget,
#'   default is "<a>:<b><c>" for line chart.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(formatter = "<a><b>:<c>")}
#' @param calculable logical whether the front-end interactive chart will
#'   support the drag-recalculable feature.
#'   the size and calculable option can be setted after line chart has been
#'   generated through eOption: \code{Echart + eOption(calculable = TRUE)}
#' @note You are recommended to use lazyPlot function for interactive chart
#'   option set through "shiny" server.
#' @export
#' @examples
#'   eLine(WorldPhones, theme=1)
#'   #mode 2 input.
#'   df2 <- data.frame(
#'     saleNum=c(10,20,30,40,50,60,70,15,25,35,45,55,65,75,25,35,45,55,65,75,85),
#'     seller=c(rep("Yellow",7), rep("Red",7), rep("White",7)),
#'   	 weekDay = c(rep(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),3)),
#'     stringsAsFactors =FALSE
#'   )
#'   eLine(df2, xvar=~weekDay, yvar= ~saleNum, series=~seller)
#'
#'  dat <- cut(rnorm(1000), -4:4)
#'  eLine(dat)
#'
#'

eLine = function(dat, xvar=NULL, yvar=NULL, series=NULL, size = NULL, horiz = FALSE,
                 theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top",
                 legend = TRUE, legend.x = "left", legend.y= "top", legend.orient="horizontal",
                 toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top",
                 dataView = TRUE, readOnly = FALSE, mark=TRUE, dataZoom=FALSE, magicType=TRUE,
                 tooltip = TRUE, tooltip.trigger="axis", formatter="", axis.scale=TRUE,
                 axis.line=TRUE, axis.tick=FALSE, axis.lable=TRUE, axis.splitLine=TRUE, axis.splitArea=FALSE, axis.boundaryGap=TRUE,
                 xlab=TRUE, xlab.type="category", xlab.data=NULL, xlab.position="bottom",
                 xlab.name = "", xlab.namePosition="start", xlim=NULL,
                 ylab=TRUE, ylab.type="value", ylab.data=NULL, ylab.position="left",
                 ylab.name = "", ylab.namePosition="start", ylim=NULL,
                 calculable=TRUE, showLabel=TRUE, opt = list())
{
  xlabName = autoArgLabel(xvar, deparse(substitute(xvar)))
  ylabName = autoArgLabel(yvar, deparse(substitute(yvar)))
  seriesName = autoArgLabel(series, deparse(substitute(series)))

  xvar = evalFormula(xvar, dat)
  yvar = evalFormula(yvar, dat)

  series = evalFormula(series, dat)

  # if series is null, we will use the xvar and yvar to construct the line plot..
  if(is.null(xvar) & is.null(yvar) & !is.factor(dat)){
    # Mode 1. use default data.frame as input...
    plotData <- as.data.frame(dat, stringsAsFactor=F)
  }else if(!is.null(xvar) & !is.null(yvar) & !is.null(series)){
    #print("Mode1")
    # Mode 2. all of xvar, yvar and series are valid...
    xvarArray = unique(as.character(xvar))
    seriesArray = unique(as.character(series))
    dataMatrix = xtabs(as.formula(paste0(ylabName, "~", xlabName , "+",  seriesName)), dat)
    plotData <- as.data.frame.matrix(dataMatrix[xvarArray,seriesArray])
  }else if(!is.null(xvar) & !is.null(yvar) & is.null(series)){
    # Mode 3. format dat with only x and y variable.
    plotData <- data.frame(val = yvar)
    colnames(plotData) <- ylabName
    rownames(plotData) <- xvar
  }else if(is.null(xvar) & is.null(yvar) & is.factor(dat)){
    # Mode 4. factor
    tempD <- as.data.frame(table(dat))
    plotData <- data.frame(val = tempD[,"Freq"])
    colnames(plotData) <- "Frequency"
    rownames(plotData) <- tempD[,1]
  }

  # option$title format.
  opt$title = tilteSet(title = title, subtitle=subtitle,
                       title.x = title.x, title.y = title.y)

  opt$calculable = calculableSet(calculable = calculable)
  opt$theme = themeSet(theme = theme)

  # opt$tooltip format, not open to user now.
  opt$tooltip = tooltipSet( tooltip=tooltip,trigger=tooltip.trigger,
                            formatter = "", islandFormatter="")

  opt$toolbox = toolboxSet(toolbox=toolbox, toolbox.x=toolbox.x, toolbox.y=toolbox.y, orient=toolbox.orient,
                           dataView=dataView, mark=mark, dataZoom = dataZoom, magicType = magicType, restore = TRUE, readOnly = readOnly,
                           saveAsImage=TRUE)


  opt$legend = legendSet( show=legend, data=colnames(plotData), legend.x=legend.x, legend.y=legend.y, orient=legend.orient)

  if(match.arg(xlab.type, c("category" , "value")) == "category" & is.null(xlab.data)){
    xlab.data = rownames(plotData)
  }
  if(match.arg(ylab.type, c("category" , "value")) == "category" & is.null(ylab.data)){
    ylab.data = colnames(plotData)
  }
  opt$xAxis = xAxisSet(axisShow=xlab, type=xlab.type, data=xlab.data, position=xlab.position,
                       labelName=xlab.name, label.namePosition=xlab.namePosition, lim=xlim,
                       axisLine=axis.line, axisTick=axis.tick, axisLable=axis.lable, splitLine=axis.splitLine,
                       splitArea=axis.splitArea, boundaryGap=axis.boundaryGap, scale=axis.scale)

  opt$yAxis = yAxisSet(axisShow=ylab, type=ylab.type, data=ylab.data, position=ylab.position,
                       labelName=ylab.name, label.namePosition=ylab.namePosition, lim=ylim,
                       axisLine=axis.line, axisTick=axis.tick, axisLable=axis.lable, splitLine=axis.splitLine,
                       splitArea=axis.splitArea, boundaryGap=axis.boundaryGap, scale=axis.scale)

  # data set...
  opt$series =  vector("list", ncol(plotData))
  for(i in 1:ncol(plotData)) {
    if(is.null(opt$series[[i]]$type)) {
      opt$series[[i]]$type = 'line'
    }
    if(is.null(opt$series[[i]]$name)) {
      opt$series[[i]]$name = colnames(plotData)[i]
    } else {
      warning('You can set series:name with colnames(dat).')
    }

    if(is.null(opt$series[[i]]$data)) {
      opt$series[[i]]$data = unnames(plotData[,i])
    } else {
      warning('You can set series:data with dat.')
    }
  }

  if(horiz==TRUE) {
    tmp = opt$xAxis
    opt$xAxis = opt$yAxis
    opt$yAxis = tmp
  }
  opt$size = size

  chart = htmlwidgets::createWidget(
    'echarts', opt,
    package = 'recharts', width = size[1], height = size[2],
    preRenderHook = function(instance) {
      instance
    }
  )
  chart = .addClass(chart, "eLine")
  chart
}

#' Create an HTML area charts widget using the ECharts(version:3.2.2) library
#'
#' This function creates an HTML widget to display matrix, data.frame and
#' factor array, using the JavaScript library ECharts3.
#' @param dat a data object (a matrix, a data frame or a factor array)
#' @param xvar,yvar objects of class "formula" (or one that can be coerced
#'   to that class):  x,y coordinates of the given data.frame colnames, e.g.
#'   \code{xvar = ~xAxisName}; \code{yvar = ~yAxisName}. xvar, yvar only needed for the
#'   data.frame data input.
#' @param series an "formula" object: Associates the levels of variable
#'   with symbol color, e.g. \code{series = ~groupName}
#' @param size an array of html widget width and height(either numeric pixels
#'   or percentage could be accepted): e.g. size = c(1024, 768).
#' @param theme an object of theme name. see(\url{http://datatables.net/extensions/index}) for detail.
#'   supported theme: \code{c("default", "vintage", "dark", "westeros", "essos", "wonderland", "walden",
#'   "chalk", "infographic", "macarons", "roma", "shine", "purple-passion")}
#' @param title an overall title for the plot. you can modify title widget after chart has been
#'   generated: Echart + eTitle(title = "your title.")
#' @param title.x,title.y the xy coordinates of main title, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify title widget after chart has been generated:
#'   \code{Echart + eTitle(title="main title", x = "left", y=10)}
#' @param legend logical whether the legend widget show or not, default is TRUE.
#'   you can modify legend widget after chart has been generated, the legend position and
#'   legend orientation are available at present.
#'   \code{Echart + eLegend(show = TRUE)}
#' @param legend.x,legend.y the xy coordinates of legend, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(x = "right", y="top")}
#' @param legend.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(orient = "vertical")}
#' @param toolbox logical whether the toolbox widget show or not, default is TRUE.
#'   you can modify toolbox widget after cahart has been generated, the toolbox position, toolbox
#'   element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(show = TRUE)}
#' @param toolbox.x,toolbox.y the xy coordinates of toolbox, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(x = "right", y="top")}
#' @param toolbox.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(orient = "vertical")}
#' @param dataview,mark,restore,dataZoom,magicType logical variable whether the dataview
#'   , mark, restore, dataZoom or magicType tool in toolbox widget show or not,
#'   default is TRUE. you can modify toolbox widget after chart has been generated,
#'   the toolbox position, toolbox element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(dataView = FALSE)}
#' @param tooltip logical whether the tooltip widget for front-end interactive chart
#'   show or not. default is TRUE. you can modify tooltip widget after chart has been generated,
#'   the tooltip trigger and tooltip formatter is available at present.
#'   \code{Echart + eTooltip(show = FALSE)}
#' @param tooltip.trigger an element of c("axis", "item"), default is "axis" for area chart.
#'   "axis" option for trigger will show all the information of mouse;
#'   "item" option for tirgger will only show the given item information of mouse.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(trigger = "axis")}
#' @param tooltip.formatter the information formatter for tooltip widget,
#'   default is "<a>:<b><c>" for area chart.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(formatter = "<a><b>:<c>")}
#' @param calculable logical whether the front-end interactive chart will
#'   support the drag-recalculable feature.
#'   the size and calculable option can be setted after area chart has been
#'   generated through eOption: \code{Echart + eOption(calculable = TRUE)}
#' @note You are recommended to use lazyPlot function for interactive chart
#'   option set through "shiny" server.
#' @export
#' @examples
#'   eArea(WorldPhones, theme=1)
#'   #mode 2 input.
#'   df2 <- data.frame(
#'     saleNum=c(10,20,30,40,50,60,70,15,25,35,45,55,65,75,25,35,45,55,65,75,85),
#'     seller=c(rep("Yellow",7), rep("Red",7), rep("White",7)),
#'   	 weekDay = c(rep(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),3)),
#'     stringsAsFactors =FALSE
#'   )
#'   eArea(df2, xvar=~weekDay, yvar= ~saleNum, series=~seller)
#'
#'  dat <- cut(rnorm(1000), -4:4)
#'  eArea(dat)
#'
#'
eArea = function(dat, xvar=NULL, yvar=NULL, series=NULL, size = NULL, horiz = FALSE, stack="SUM",
                 theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top",
                 legend = TRUE, legend.x = "left", legend.y= "top", legend.orient="horizontal",
                 toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top",
                 dataView = TRUE, readOnly = FALSE, mark=TRUE, dataZoom=FALSE, magicType=TRUE,
                 tooltip = TRUE, tooltip.trigger="item", formatter="", axis.scale=FALSE,
                 axis.line=TRUE, axis.tick=FALSE, axis.lable=TRUE, axis.splitLine=TRUE, axis.splitArea=FALSE, axis.boundaryGap=TRUE,
                 xlab=TRUE, xlab.type="category", xlab.data=NULL, xlab.position="bottom",
                 xlab.name = "", xlab.namePosition="start", xlim=NULL,
                 ylab=TRUE, ylab.type="value", ylab.data=NULL, ylab.position="left",
                 ylab.name = "", ylab.namePosition="start", ylim=NULL,
                 calculable=TRUE, showLabel=TRUE, opt = list())
{

  xlabName = autoArgLabel(xvar, deparse(substitute(xvar)))
  ylabName = autoArgLabel(yvar, deparse(substitute(yvar)))
  seriesName = autoArgLabel(series, deparse(substitute(series)))

  xvar = evalFormula(xvar, dat)

  yvar = evalFormula(yvar, dat)

  series = as.factor(evalFormula(series, dat))
  # if series is null, we will use the xvar and yvar to construct the area plot..
  if(is.null(xvar) & is.null(yvar) & !is.factor(dat)){
    # Mode 1. use default data.frame as input...
    plotData = as.data.frame(dat, stringsAsFactor=F)
  }else if(!is.null(xvar) & !is.null(yvar) & !is.null(series)){
    #print("Mode1")
    # Mode 2. all of xvar, yvar and series are valid...
    xvarArray = unique(as.character(xvar))
    seriesArray = unique(as.character(series))
    dataMatrix = xtabs(as.formula(paste0(ylabName, "~", xlabName , "+",  seriesName)), dat)
    plotData <- as.data.frame.matrix(dataMatrix[xvarArray,seriesArray])
  }else if(!is.null(xvar) & !is.null(yvar) & is.null(series)){
    # Mode 3. format dat with only x and y variable.
    plotData = data.frame(val = yvar)
    colnames(plotData) = ylabName
    rownames(plotData) = xvar
  }else if(is.null(xvar) & is.null(yvar) & is.factor(dat)){
    # Mode 4. factor
    tempD = as.data.frame(table(dat))
    plotData = data.frame(val = tempD[,"Freq"])
    colnames(plotData) = "Frequency"
    rownames(plotData) = tempD[,1]
  }


  # option$title format.
  opt$title = tilteSet(title = title, subtitle=subtitle,
                       title.x = title.x, title.y = title.y)

  opt$calculable = calculableSet(calculable = calculable)

  # opt$tooltip format, not open to user now.
  opt$tooltip = tooltipSet( tooltip=tooltip,trigger=tooltip.trigger,
                            formatter = "", islandFormatter="")

  opt$toolbox = toolboxSet(toolbox=toolbox, toolbox.x=toolbox.x, toolbox.y=toolbox.y, orient=toolbox.orient,
                           dataView=dataView, mark=mark, dataZoom = dataZoom, magicType = magicType, restore = TRUE, readOnly = readOnly,
                           saveAsImage=TRUE)


  opt$legend = legendSet( show=legend, data=colnames(plotData), legend.x=legend.x, legend.y=legend.y, orient=legend.orient)

  if(match.arg(xlab.type, c("category" , "value")) == "category" & is.null(xlab.data)){
    xlab.data = rownames(plotData)
  }
  if(match.arg(ylab.type, c("category" , "value")) == "category" & is.null(ylab.data)){
    ylab.data = colnames(plotData)
  }
  opt$xAxis = xAxisSet(axisShow=xlab, type=xlab.type, data=xlab.data, position=xlab.position,
                       labelName=xlab.name, label.namePosition=xlab.namePosition, lim=xlim,
                       axisLine=axis.line, axisTick=axis.tick, axisLable=axis.lable, splitLine=axis.splitLine,
                       splitArea=axis.splitArea, boundaryGap=axis.boundaryGap, scale=axis.scale)

  opt$yAxis = yAxisSet(axisShow=ylab, type=ylab.type, data=ylab.data, position=ylab.position,
                       labelName=ylab.name, label.namePosition=ylab.namePosition, lim=ylim,
                       axisLine=axis.line, axisTick=axis.tick, axisLable=axis.lable, splitLine=axis.splitLine,
                       splitArea=axis.splitArea, boundaryGap=axis.boundaryGap, scale=axis.scale)

  # data set...
  opt$series =  vector("list", ncol(plotData))

  for(i in 1:ncol(plotData)) {
    if(is.null(opt$series[[i]]$type)) {
      opt$series[[i]]$type = 'line'
    }

    if(is.null(opt$series[[i]]$name)) {
      opt$series[[i]]$name = colnames(plotData)[i]
    } else {
      warning('You can set series:name with dat.')
    }

    if(is.null(opt$series[[i]]$data)) {
      opt$series[[i]]$data = unnames(plotData[,i])
    } else {
      warning('You can set series:data with dat.')
    }

    if(is.null(opt$series[[i]]$stack)) {
      opt$series[[i]]$stack = 'SUM'
    }

    if(is.null(opt$series[[i]]$itemStyle$normal$areaStyle$type)) {
      opt$series[[i]]$itemStyle$normal$areaStyle$type = 'default'
    }
  }


  if(horiz==TRUE) {
    tmp = opt$xAxis
    opt$xAxis = opt$yAxis
    opt$yAxis = tmp
  }

  opt$size = size

  chart = htmlwidgets::createWidget(
    'echarts', opt,
    package = 'recharts', width = opt$size[1], height = opt$size[2],
    preRenderHook = function(instance) {
      instance
    }
  )
  chart = .addClass(chart, "eArea")
  chart

}


#' Create an HTML bar charts widget using the ECharts(version:3.2.2) library
#'
#' This function creates an HTML widget to display matrix, data.frame and
#' factor array, using the JavaScript library ECharts3.
#' @param dat a data object (a matrix, a data frame or a factor array)
#' @param xvar,yvar objects of class "formula" (or one that can be coerced
#'   to that class):  x,y coordinates of the given data.frame colnames, e.g.
#'   \code{xvar = ~xAxisName}; \code{yvar = ~yAxisName}. xvar, yvar only needed for the
#'   data.frame data input.
#' @param series an "formula" object: Associates the levels of variable
#'   with symbol color, e.g. \code{series = ~groupName}
#' @param size an array of html widget width and height(either numeric pixels
#'   or percentage could be accepted): e.g. size = c(1024, 768).
#' @param horiz logical. If FALSE, the bars are drawn vertically with the
#'   first bar to the left.
#' @param stackGroup list object, used to make series pre-stacked before rendering
#'   whole bar chart, if \code{unique(df[["groupName"]])} = c("a", "b", "c", "d",
#'   "e", "f") and an example legal input for stackGroup should be:
#'   \code{stackGroup = list(c("a","b"), c("e","f"))}. And the c("a","b") and
#'   c("e","f") will be stacked into two bar, and the "c" and "d" will account
#'   two seperated bars.
#' @param  theme an object of theme name. see(\url{http://datatables.net/extensions/index}) for detail.
#'   supported theme: \code{c("default", "vintage", "dark", "westeros", "essos", "wonderland", "walden",
#'   "chalk", "infographic", "macarons", "roma", "shine", "purple-passion")}
#' @param title an overall title for the plot. you can modify title widget after chart has been
#'   generated: barEchart + eTitle(title = "your title.")
#' @param title.x,title.y the xy coordinates of main title, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify title widget after chart has been generated:
#'   \code{barEchart + eTitle(title="main title", x = "left", y=10)}
#' @param legend logical whether the legend widget show or not, default is TRUE.
#'   you can modify legend widget after chart has been generated, the legend position and
#'   legend orientation are available at present.
#'   \code{barEchart + eLegend(show = TRUE)}
#' @param legend.x,legend.y the xy coordinates of legend, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify legend widget after chart has been generated:
#'   \code{barEchart + eLegend(x = "right", y="top")}
#' @param legend.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify legend widget after chart has been generated:
#'   \code{barEchart + eLegend(orient = "vertical")}
#' @param toolbox logical whether the toolbox widget show or not, default is TRUE.
#'   you can modify toolbox widget after chart has been generated, the toolbox position, toolbox
#'   element and toolbox orientation are available at present.
#'   \code{barEchart + eToolbox(show = TRUE)}
#' @param toolbox.x,toolbox.y the xy coordinates of toolbox, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify toolbox widget after chart has been generated:
#'   \code{barEchart + eToolbox(x = "right", y="top")}
#' @param toolbox.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify toolbox widget after chart has been generated:
#'   \code{barEchart + eToolbox(orient = "vertical")}
#' @param dataview,mark,restore,dataZoom,magicType logical variable whether the dataview
#'   , mark, restore, dataZoom or magicType tool in toolbox widget show or not,
#'   default is TRUE. you can modify toolbox widget after chart has been generated,
#'   the toolbox position, toolbox element and toolbox orientation are available at present.
#'   \code{barEchart + eToolbox(dataView = FALSE)}
#' @param tooltip logical whether the tooltip widget for front-end interactive chart
#'   show or not. default is TRUE. you can modify tooltip widget after chart has been generated,
#'   the tooltip trigger and tooltip formatter is available at present.
#'   \code{barEchart + eTooltip(show = FALSE)}
#' @param tooltip.trigger an element of c("axis", "item"), default is "axis" for bar chart.
#'   "axis" option for trigger will show all the information of mouse;
#'   "item" option for tirgger will only show the given item information of mouse.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{barEchart + eTooltip(trigger = "axis")}
#' @param tooltip.formatter the information formatter for tooltip widget,
#'   default is "<a>:<b><c>" for bar chart.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{barEchart + eTooltip(formatter = "<a><b>:<c>")}
#' @param calculable logical whether the front-end interactive chart will
#'   support the drag-recalculable feature.
#'   the size and calculable option can be setted after bar chart has been
#'   generated through eOption: \code{barEchart + eOption(calculable = TRUE)}
#' @note You are recommended to use lazyPlot function for interactive chart
#'   option set through "shiny" server.
#' @export
#' @examples require(plyr)
#' dat = ddply(iris, .(Species), colwise(mean))
#' rownames(dat) = dat[,1]
#' dat = dat[, -1]
#' dat
#' eBar(dat)
#' eBar(dat, horiz = TRUE)
#' #mode 2 input.
#' df2 <- data.frame(
#'  saleNum=c(10,20,30,40,50,60,70,15,25,35,45,55,65,75,25,35,45,55,65,75,85),
#'  seller=c(rep("Yellow",7), rep("Red",7), rep("White",7)),
#'	 weekDay = c(rep(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),3)),
#'  stringsAsFactors =FALSE
#' )
#' dat <- df2
#' xvar=~weekDay; yvar= ~saleNum; series=~seller
#' eBar(df2, xvar = ~seller, ~saleNum, ~weekDay )
#' dat <- df2[1:7,]
#' eBar(dat, ~weekDay, ~saleNum)
#' dat <- cut(rnorm(1000), -4:4)
#' eBar(dat)
#'

eBar = function(dat, xvar=NULL, yvar=NULL, series=NULL, size = NULL, horiz = FALSE, stackGroup = NULL,
                theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top",
                legend = TRUE, legend.x = "left", legend.y= "top", legend.orient="horizontal",
                toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top",
                dataView = TRUE, readOnly = FALSE, mark=TRUE, dataZoom=FALSE, magicType=TRUE,
                tooltip = TRUE, tooltip.trigger="axis", tooltip.formatter="", axis.scale=TRUE,
                axis.line=TRUE, axis.tick=FALSE, axis.lable=TRUE, axis.splitLine=TRUE, axis.splitArea=FALSE, axis.boundaryGap=TRUE,
                xlab=TRUE, xlab.type="category", xlab.data=NULL, xlab.position="bottom",
                xlab.name = "", xlab.namePosition="start", xlim=NULL,
                ylab=TRUE, ylab.type="value", ylab.data=NULL, ylab.position="left",
                ylab.name = "", ylab.namePosition="start", ylim=NULL,
                calculable=TRUE, showLabel=TRUE, opt = list())
{
  xlabName = autoArgLabel(xvar, deparse(substitute(xvar)))
  ylabName = autoArgLabel(yvar, deparse(substitute(yvar)))

  xvar = evalFormula(xvar, dat)
  yvar = evalFormula(yvar, dat)
  seriesName = autoArgLabel(series, deparse(substitute(series)))
  if (!is.null(series)) series = as.factor(as.character(evalFormula(series, dat)))

  # if series is null, we will use the xvar and yvar to construct the bar plot..
  if(is.null(xvar) & is.null(yvar) & !is.factor(dat)){
    # Mode 1. use default data.frame as input...
    plotData <- as.data.frame(dat, stringsAsFactor=F)
  }else if(!is.null(xvar) & !is.null(yvar) & !is.null(series)){
    # print("Mode1")
    # Mode 2. all of xvar, yvar and series are valid...
    xvarArray = unique(as.character(xvar))
    seriesArray = unique(as.character(series))
    dataMatrix = xtabs(as.formula(paste0(ylabName, "~", xlabName , "+",  seriesName)), dat)
    plotData <- as.data.frame.matrix(dataMatrix[xvarArray,seriesArray])
  }else if(!is.null(xvar) & !is.null(yvar) & is.null(series)){
    # Mode 3. format dat with only x and y variable.
    plotData <- data.frame(val = yvar)
    colnames(plotData) <- ylabName
    rownames(plotData) <- xvar
  }else if(is.null(xvar) & is.null(yvar) & is.factor(dat)){
    # Mode 4. factor
    tempD <- as.data.frame(table(dat))
    plotData <- data.frame(val = tempD[,"Freq"])
    colnames(plotData) <- "Frequency"
    rownames(plotData) <- tempD[,1]
  }

  #opt = list()

  # option$title format.
  opt$title = tilteSet(title = title, subtitle=subtitle,
                       title.x = title.x, title.y = title.y)

  opt$calculable = calculableSet(calculable = calculable)
  opt$theme = themeSet(theme = theme)

  # opt$tooltip format, not open to user now.
  opt$tooltip = tooltipSet(tooltip=tooltip,trigger=tooltip.trigger,
                           formatter = tooltip.formatter, islandFormatter="")

  opt$toolbox = toolboxSet(toolbox=toolbox, toolbox.x=toolbox.x, toolbox.y=toolbox.y, orient=toolbox.orient,
                           dataView=dataView, mark=mark, dataZoom = dataZoom, magicType = magicType, restore = TRUE, readOnly = readOnly,
                           saveAsImage=TRUE)

  opt$legend = legendSet( show=legend, data=colnames(plotData), legend.x=legend.x, legend.y=legend.y, orient=legend.orient)

  if(match.arg(xlab.type, c("category" , "value")) == "category" & is.null(xlab.data)){
    xlab.data = rownames(plotData)
  }
  if(match.arg(ylab.type, c("category" , "value")) == "category" & is.null(ylab.data)){
    ylab.data = colnames(plotData)
  }

  opt$xAxis = xAxisSet(axisShow=xlab, type=xlab.type, data=xlab.data, position=xlab.position,
                       labelName=xlab.name, label.namePosition=xlab.namePosition, lim=xlim,
                       axisLine=axis.line, axisTick=axis.tick, axisLable=axis.lable, splitLine=axis.splitLine,
                       splitArea=axis.splitArea, boundaryGap=axis.boundaryGap, scale=axis.scale)

  opt$yAxis = yAxisSet(axisShow=ylab, type=ylab.type, data=ylab.data, position=ylab.position,
                       labelName=ylab.name, label.namePosition=ylab.namePosition, lim=ylim,
                       axisLine=axis.line, axisTick=axis.tick, axisLable=axis.lable, splitLine=axis.splitLine,
                       splitArea=axis.splitArea, boundaryGap=axis.boundaryGap, scale=axis.scale)

  # data set...
  opt$series =  vector("list", ncol(plotData))
  for(i in 1:dim(plotData)[2]) {
    if(is.null(opt$series[[i]]$type)) {
      opt$series[[i]]$type = 'bar'
    }

    if(is.null(opt$series[[i]]$name)) {
      opt$series[[i]]$name = colnames(plotData)[i]
    } else {
      warning('You can set series:name with colnames(dat).')
    }

    if(is.null(opt$series[[i]]$data)) {
      opt$series[[i]]$data = unname(plotData[,i])
    } else {
      warning('You can set series:data with dat.')
    }
  }
  if(horiz==TRUE) {
    tmp = opt$xAxis
    opt$xAxis = opt$yAxis
    opt$yAxis = tmp
  }
  #jsonStr <- toJSON(opt, pretty=TRUE)
  #outList <- .rechartsOutput(jsonStr, charttype="ePoints", size=size)
  opt$size = size


  chart = htmlwidgets::createWidget(
    'echarts', opt,
    package = 'recharts', width = size[1], height = size[2],
    preRenderHook = function(instance) {
      instance
    }
  )
  chart = .addClass(chart, "eBar")
  chart
  ##### output list format
  # chart = htmlwidgets::createWidget(
  # 	'echarts', opt, width = size[1], height = size[2], package = 'recharts'
  # )
  # chart
}




#' Create the K chart
#' @export
eK = function( data = NULL, x = NULL, type = 'k',
                    width = NULL, height = NULL, ...) {

  if (is.null(x))
    x = seq(nrow(data))
  else
    x = evalFormula(x, data)      ## FIXME: Find the Date/DateTime information, and reformat.

  y = data[,c("o","c","l","h")] ## FIXME: Find the ohlc price in data

  params = structure(list(
    series = data_K(x, y),
    xAxis = list(type='category',data=x), yAxis = list(type='value')
  ), meta = list(
    x = x, y = y
  ))

  chart = htmlwidgets::createWidget(
    'echarts', params, width = width, height = height, package = 'recharts',
    dependencies = getDependency(NULL)
  )

  chart %>% tooltip(trigger='axis') %>% dataZoom() %>% toolbox()
}


#' Create an HTML pie charts widget using the ECharts(version:3.2.2) library
#'
#' This function creates an HTML widget to display data.frame and
#' numeric array with name, using the JavaScript library ECharts3.
#' @param dat a data object (a data frame or a factor array)
#' @param namevar objects of class "formula" (or one that can be coerced
#'   to that class):  the pie name of the given data.frame colnames, e.g.
#'   \code{namevar = ~pieName};  namevar only needed for the
#'   data.frame data input.
#' @param datavar an "formula" object: Associates the value of variable
#'   with symbol color, e.g. \code{datavar = ~valueName}.
#' @param size an array of html widget width and height(either numeric pixels
#'   or percentage could be accepted): e.g. size = c(1024, 768).
#' @param type an option of c("pie", "rose"), the pie type of diplay widgets.
#' @param roseType if the pie chart type is rose, this option will be available.
#'   and this option input should be one element of c("radias", "area").
#' @param  theme an object of theme name. see(\url{http://datatables.net/extensions/index}) for detail.
#'   supported theme: \code{c("default", "vintage", "dark", "westeros", "essos", "wonderland", "walden",
#'   "chalk", "infographic", "macarons", "roma", "shine", "purple-passion")}
#' @param title an overall title for the plot. you can modify title widget after chart has been
#'   generated: Echart + eTitle(title = "your title.")
#' @param title.x,title.y the xy coordinates of main title, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify title widget after chart has been generated:
#'   \code{Echart + eTitle(title="main title", x = "left", y=10)}
#' @param legend logical whether the legend widget show or not, default is TRUE.
#'   you can modify legend widget after chart has been generated, the legend position and
#'   legend orientation are available at present.
#'   \code{Echart + eLegend(show = TRUE)}
#' @param legend.x,legend.y the xy coordinates of legend, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(x = "right", y="top")}
#' @param legend.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(orient = "vertical")}
#' @param toolbox logical whether the toolbox widget show or not, default is TRUE.
#'   you can modify toolbox widget after chart has been generated, the toolbox position, toolbox
#'   element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(show = TRUE)}
#' @param toolbox.x,toolbox.y the xy coordinates of toolbox, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(x = "right", y="top")}
#' @param toolbox.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(orient = "vertical")}
#' @param dataview,mark,restore,dataZoom,magicType logical variable whether the dataview
#'   , mark, restore, dataZoom or magicType tool in toolbox widget show or not,
#'   default is TRUE. you can modify toolbox widget after chart has been generated,
#'   the toolbox position, toolbox element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(dataView = FALSE)}
#' @param tooltip logical whether the tooltip widget for front-end interactive chart
#'   show or not. default is TRUE. you can modify tooltip widget after chart has been generated,
#'   the tooltip trigger and tooltip formatter is available at present.
#'   \code{Echart + eTooltip(show = FALSE)}
#' @param tooltip.trigger an element of c("axis", "item"), default is "axis" for bar chart.
#'   "axis" option for trigger will show all the information of mouse;
#'   "item" option for tirgger will only show the given item information of mouse.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(trigger = "axis")}
#' @param tooltip.formatter the information formatter for tooltip widget,
#'   default is "<a>:<b><c>" for bar chart.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(formatter = "<a><b>:<c>")}
#' @param calculable logical whether the front-end interactive chart will
#'   support the drag-recalculable feature.
#'   the size and calculable option can be setted after bar chart has been
#'   generated through eOption: \code{Echart + eOption(calculable = TRUE)}
#' @param showLabel logical whether the region name label showed on chart.
#'   default is TRUE, e.g. \code{Echart + eOption(showLabel = TRUE)}
#' @note You are recommended to use lazyPlot function for interactive chart
#'   option set through "shiny" server.
#' @export
#' @examples
#' x = runif(6)
#' names(x) = LETTERS[1:6]
#' ePie(x) + eTitle("test")
#' testData <- head(mapTestData_chs, 5)
#' ePie(testData, ~stdName, ~val1)
ePie = function(dat, namevar=NULL, datavar=NULL, size = NULL,  type=c("pie", "rose"), roseType=c("radias", "area"),
                theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top",
                legend = TRUE, legend.x = "left", legend.y= "top", legend.orient="horizontal",
                toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top",
                dataView = TRUE, readOnly = FALSE, mark=TRUE, dataZoom=FALSE, magicType=FALSE,
                tooltip = TRUE, tooltip.trigger="item", formatter="", axis.scale=TRUE,
                xlab=FALSE, ylab=FALSE,	calculable=TRUE, showLabel=TRUE, opt = list())
{
  type <- match.arg(type)
  roseType <- match.arg(roseType)
  # if the input is an array or a vector, will use the names as the pie name,
  # and use the value as the pie value
  if(is.vector(dat) || is.array(dat)){
    dat <- as.data.frame(dat)
    datavar <- 1
    dat$namevar <- rownames(dat)
    namevar <- "namevar"
  }
  else{
    # if the input dat is not data.frame will format it into data.frame.
    if (class(dat) != "data.frame") dat <- as.data.frame(dat)

    # if the user input argument namevar is null, will use colume one as name input
    if(is.null(namevar)){
      namevar <- 1
    }else{
      namevar = autoArgLabel(namevar, deparse(substitute(namevar)))
      namevar = evalFormula(namevar, data)
    }
    if(is.null(datavar)){
      datavar <- 2
    }else{
      datavar = autoArgLabel(datavar, deparse(substitute(datavar)))
      datavar = evalFormula(datavar, data)
    }
  }

  # option$title format.
  opt$title = tilteSet(title = title, subtitle=subtitle,
                       title.x = title.x, title.y = title.y)

  opt$calculable = calculableSet(calculable = calculable)
  opt$theme = themeSet(theme = theme)

  # opt$tooltip format, not open to user now.
  opt$tooltip = tooltipSet( tooltip=tooltip,trigger=tooltip.trigger,
                            formatter = "", islandFormatter="")

  opt$toolbox = toolboxSet(toolbox=toolbox, toolbox.x=toolbox.x, toolbox.y=toolbox.y, orient=toolbox.orient,
                           dataView=dataView, mark=mark, dataZoom = dataZoom, magicType = magicType, restore = TRUE, readOnly = readOnly,
                           saveAsImage=TRUE)


  opt$legend = legendSet( show=legend, data=dat[[namevar]], legend.x=legend.x, legend.y=legend.y, orient=legend.orient)

  datFrame = data.frame(value=dat[[datavar]], name=dat[[namevar]])
  datList = lapply(split(datFrame, seq_len(nrow(datFrame))), as.list)
  names(datList) = NULL

  #showLabelLine=showLabel
  #now we don't support the multiple graph in one canvas
  opt$series = list(
    list(
      name = paste(type, "chart"),
      type = "pie",
      radius = c(20,110),
      center = c("50%", 200),
      roseType = ifelse(type=="rose", roseType, ""),
      itemStyle = list(
        normal = list(
          label = list( show = showLabel),
          labelLine = list( show = showLabel)
        ),
        emphasis = list(
          label = list( show = !showLabel),
          labelLine = list( show = !showLabel)
        )
      ),
      data = datList
    )
  )

  #jsonStr <- toJSON(opt, pretty=TRUE)
  #outList <- .rechartsOutput(jsonStr, charttype="ePie", size=size)
  opt$size = size

  ### output list format
  chart = htmlwidgets::createWidget(
    'echarts', opt, width = size[1], height = size[2], package = 'recharts'
  )
  chart = .addClass(chart, "ePie")
  chart
}



#' Create an HTML radar charts widget using the ECharts(version:3.2.2) library
#'
#' This function creates an HTML widget to display data.frame using the JavaScript library ECharts3.
#' @param dat a data object (a data frame or a factor array)
#' @param xvar,yvar objects of class "formula" (or one that can be coerced
#'   to that class):  x,y coordinates of the given data.frame colnames, e.g.
#'   \code{xvar = ~xAxisName}; \code{yvar = ~yAxisName}. xvar, yvar are needed for the
#'   data.frame data input.
#' @param series an "formula" object: Associates the levels of variable
#'   with symbol color, e.g. \code{series = ~groupName}
#' @param size an array of html widget width and height(either numeric pixels
#'   or percentage could be accepted): e.g. size = c(1024, 768).
#' @param ymin,ymax a numeric array with the same length of unique(xvar), to set the
#'   limitation for each polar axis limitation. if you want to set someone to be default
#'   limitation, you can set the given array index to NULL, examples input:
#'   \code{ymin = c(NULL, 1000, 2000, 3000)}
#' @param  theme an object of theme name. see(\url{http://datatables.net/extensions/index}) for detail.
#'   supported theme: \code{c("default", "vintage", "dark", "westeros", "essos", "wonderland", "walden",
#'   "chalk", "infographic", "macarons", "roma", "shine", "purple-passion")}
#' @param title an overall title for the plot. you can modify title widget after chart has been
#'   generated: Echart + eTitle(title = "your title.")
#' @param title.x,title.y the xy coordinates of main title, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify title widget after chart has been generated:
#'   \code{Echart + eTitle(title="main title", x = "left", y=10)}
#' @param legend logical whether the legend widget show or not, default is TRUE.
#'   you can modify legend widget after chart has been generated, the legend position and
#'   legend orientation are available at present.
#'   \code{Echart + eLegend(show = TRUE)}
#' @param legend.x,legend.y the xy coordinates of legend, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(x = "right", y="top")}
#' @param legend.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(orient = "vertical")}
#' @param toolbox logical whether the toolbox widget show or not, default is TRUE.
#'   you can modify toolbox widget after chart has been generated, the toolbox position, toolbox
#'   element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(show = TRUE)}
#' @param toolbox.x,toolbox.y the xy coordinates of toolbox, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(x = "right", y="top")}
#' @param toolbox.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(orient = "vertical")}
#' @param dataview,mark,restore,dataZoom,magicType logical variable whether the dataview
#'   , mark, restore, dataZoom or magicType tool in toolbox widget show or not,
#'   default is TRUE. you can modify toolbox widget after chart has been generated,
#'   the toolbox position, toolbox element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(dataView = FALSE)}
#' @param tooltip logical whether the tooltip widget for front-end interactive chart
#'   show or not. default is TRUE. you can modify tooltip widget after chart has been generated,
#'   the tooltip trigger and tooltip formatter is available at present.
#'   \code{Echart + eTooltip(show = FALSE)}
#' @param tooltip.trigger an element of c("axis", "item"), default is "axis" for radar chart.
#'   "axis" option for trigger will show all the information of mouse;
#'   "item" option for tirgger will only show the given item information of mouse.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(trigger = "axis")}
#' @param tooltip.formatter the information formatter for tooltip widget,
#'   default is "<a>:<b><c>" for radar chart.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(formatter = "<a><b>:<c>")}
#' @param calculable logical whether the front-end interactive chart will
#'   support the drag-recalculable feature.
#'   the size and calculable option can be setted after radar chart has been
#'   generated through eOption: \code{Echart + eOption(calculable = TRUE)}
#' @param showLabel logical whether the region name label showed on chart.
#'   default is TRUE, e.g. \code{Echart + eOption(showLabel = TRUE)}
#' @note You are recommended to use lazyPlot function for interactive chart
#'   option set through "shiny" server.
#' @export
#' @examples
#' require(plyr)
#' dat = ddply(iris, .(Species), colwise(mean))
#' rownames(dat) = dat[,1]
#' dat = dat[, -1]
#' dat
#' eRadar(dat)
#' df2 <- data.frame(
#'  saleNum=c(10,20,30,40,50,60,70,15,25,35,45,55,65,75,25,35,45,55,65,75,85),
#' 	seller=c(rep("Yellow",7), rep("Red",7), rep("White",7)),
#'	weekDay = c(rep(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),3))
#' )
#' dat <- df2
#' xvar=~weekDay; yvar= ~saleNum; series=~seller
#' eRadar(df2, ~weekDay, ~saleNum, ~seller)
eRadar = function(dat, xvar=NULL, yvar=NULL, series=NULL, size = NULL, ymin=vector(), ymax=vector(),
                  theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top",
                  legend = TRUE, legend.data=NULL, legend.x = "left", legend.y= "top", legend.orient="horizontal",
                  toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top",
                  dataView = TRUE, readOnly = FALSE, mark=TRUE, dataZoom=FALSE, magicType=FALSE,
                  tooltip = TRUE, tooltip.trigger="item", formatter="", axis.scale=TRUE,
                  xlab=FALSE, ylab=FALSE,	calculable=TRUE, showLabel=TRUE, opt = list())
{
  xlabName = autoArgLabel(xvar, deparse(substitute(xvar)))
  ylabName = autoArgLabel(yvar, deparse(substitute(yvar)))

  xvar = as.factor(evalFormula(xvar, dat))
  yvar = evalFormula(yvar, dat)

  series = as.factor(evalFormula(series, dat))

  # data flow format..
  # if series is null, we will use the xvar and yvar to construct the bar plot..
  if(is.null(xvar) & is.null(yvar) & !is.factor(dat)){
    # Mode 1. use default data.frame as input...
    dat <- as.data.frame(dat, stringsAsFactor=F)
  }else if(!is.null(xvar) & !is.null(yvar) & !is.null(series)){
    #print("Mode1")
    # Mode 2. all of xvar, yvar and series are valid...
    dat <- with(dat, {
      out <- matrix(nrow=nlevels(series), ncol=nlevels(xvar),
                    dimnames=list(levels(series), levels(xvar)))
      out[cbind(series, xvar)] <- yvar
      out
    })
    dat <- as.data.frame(dat)
  }else if(!is.null(xvar) & !is.null(yvar) & is.null(series)){
    # Mode 3. format dat with only x and y variable.
    dat <- data.frame(val = yvar)
    colnames(dat) <- ylabName
    rownames(dat) <- xvar
  }else if(is.null(xvar) & is.null(yvar) & is.factor(dat)){
    # Mode 4. factor
    tempD <- as.data.frame(table(dat))
    dat <- data.frame(val = tempD[,"Freq"])
    colnames(dat) <- "Frequency"
    rownames(dat) <- tempD[,1]
  }


  # option$title format.
  opt$title = eTitleSet(title = title, subtitle=subtitle,
                       title.x = title.x, title.y = title.y)

  opt$calculable = calculableSet(calculable = calculable)
  opt$theme = themeSet(theme = theme)

  # opt$tooltip format, not open to user now.
  opt$tooltip = tooltipSet( tooltip=tooltip,trigger=tooltip.trigger,
                            formatter = "", islandFormatter="")

  opt$toolbox = toolboxSet(toolbox=toolbox, toolbox.x=toolbox.x, toolbox.y=toolbox.y, orient=toolbox.orient,
                           dataView=dataView, mark=mark, dataZoom = dataZoom, magicType = magicType, restore = TRUE, readOnly = readOnly,
                           saveAsImage=TRUE)

  if(missing(legend.data) | is.null(legend.data)){legendData = rownames(dat)
  }else{legendData = legend.data}

  opt$legend = legendSet( show=legend, data=legendData, legend.x=legend.x, legend.y=legend.y, orient=legend.orient)

  opt$polar = list(polarSet(name=colnames(dat), ymin=ymin, ymax=ymax))

  datList = vector("list", nrow(dat))
  for(i in 1:nrow(dat)){
    datList[[i]]$name  = rownames(dat)[i]
    datList[[i]]$value = unnames(unlist(dat[i,]))
  }
  names(datList) = NULL


  if(is.null(opt$series)) {
    opt$series = vector("list", 1)
  }
  if(is.null(opt$series[[1]]$type)) {
    opt$series[[1]]$type =  'radar'
  }

  if(is.null(opt$series[[1]]$data)) {
    opt$series[[1]]$data = datList
  }

  opt$size = size

  ### output list format
  chart = htmlwidgets::createWidget(
    'echarts', opt,
    package = 'recharts', width = size[1], height = size[2],
    preRenderHook = function(instance) {
      instance
    }
  )
  chart = .addClass(chart, "eRadar")
  chart
}


eChord = function(chart, ...) {

}

#' Force network graph
#'
#' ECharts style 力导向图 graph visulize the social network matrix data.
#'
#' @param networkMatrix   required, a symmetric matrix, each cell value indicates
#' the weight of the two nodes and the 0 or NA cell would not be counted in.
#' The matrix should have colnames or rownames.
#' @param propertyDf   optional, dataframe which contain the metadata for the nodes.
#' It could contain category, value and color columns. The colnames and rownames are required.
#' @param opt 力导向图选项.
#' @return The HTML code as a character string.
#' @export
#' @examples
#' testData <- matrix(1:25, nrow=5) #测试中文
#' eForce(testData)





#####################################
##  The network graph:
##	       Jobs(10)
##	      //1    \\2
##       //    3  \\
##    Gates(9)----Obama(8)
##
##  The weighted network Matrix would be:
##        Jobs   Gates  Obama
##  Jobs   0       1      2
##  Gates  1       0      3
##  Obama  2       3      0
##
##  The property data.frame:
##         category   value    color
##  Jobs   "人物"       10   '#ff7f50'
##  Gates  "朋友"       8    '#87cdfa'
##  Obama  "朋友"       9    '#87cdfa'
#######################################



# networkMatrix <- matrix(c(
# 	c(0, 1, 2, 1, 2, 3, 6, 6, 1, 1, 1 ),
# 	c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
#	c(2, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0 ),
#	c(1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0 ),
#	c(2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ),
#	c(3, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ),
#	c(6, 0, 1, 1, 1, 1, 0, 6, 0, 1, 0 ),
#	c(6, 0, 0, 1, 0, 0, 6, 0, 0, 0, 0 ),
#	c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
#	c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ),
#	c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
#	), ncol=11
# )

# propertyDf <- data.frame(
#	category = c("人物", "家人", "家人", "家人", "家人", "朋友",
#				"朋友", "朋友", "朋友", "朋友", "朋友"),
#	name = c("Steven Jobs", "Lisa Jobs", "Paul Jobs", " Kalala Jobs",
#			"Lauren Powell", "Steve woz Ike", "Obama", "Bill Gates",
# 			"Jonathan", "Tim Cook", "Wayne"),
#	value = c(10, 2, 3, 3, 7, 5, 8, 9, 4, 4, 0)
#  )

# rownames(propertyDf) = propertyDf$name

# eForce(networkMatrix=networkMatrix, propertyDf=propertyDf)


eForce = function(networkMatrix, propertyDf=NULL, size = NULL,
                  maxR=25, minR=15, density=0.05, attractiveness=1.2, showLabel=TRUE,
                  theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top",
                  legend = TRUE, legend.x = "left", legend.y= "top", legend.orient="horizontal",
                  toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top",
                  dataView = FALSE, readOnly = TRUE, mark=TRUE, dataZoom=FALSE,
                  tooltip = TRUE, tooltip.trigger="item", formatter="",
                  calculable=FALSE, xlab = NULL, ylab=NULL, opt = list() ) {
  ## networkMatrix would be a symmetric matrix
  ## if the propertyDf is null, all the category and value are 0 as default.

  # option$title format.
  opt$title = tilteSet(title = title, subtitle=subtitle,
                       title.x = title.x, title.y = title.y)


  opt$calculable = calculableSet(calculable = calculable)
  opt$theme = themeSet(theme = theme)

  # opt$tooltip format, not open to user now.
  opt$tooltip = tooltipSet( tooltip=tooltip,trigger=tooltip.trigger,
                            formatter = "", islandFormatter="")



  opt$toolbox = toolboxSet(toolbox=toolbox, toolbox.x=toolbox.x, toolbox.y=toolbox.y, orient=toolbox.orient,
                           dataView=dataView, mark=mark, dataZoom = dataZoom, magicType = FALSE, restore = TRUE, readOnly = readOnly,
                           saveAsImage=TRUE)

  ### data format and data map.
  if(!is.null(propertyDf) && (nrow(propertyDf) != nrow(networkMatrix))){
    warning("dat matrix doesn't have the same length to propertyDf. The propertyDf will be ignored.")
    propertyDf = NULL
  }


  networkMatrix <- as.matrix(networkMatrix)
  if (nrow(networkMatrix) != ncol(networkMatrix))  stop("networkMatrix would be a symmetric matrix")

  # matrix name check.
  if (is.null(colnames(networkMatrix))){
    if (is.null(rownames(networkMatrix))){
      if (is.null(propertyDf)){
        # if the rowname, colname and the propertyDf are missing, will use 1:nrow as names.
        rownames(networkMatrix) = 1:nrow(networkMatrix)
        colnames(networkMatrix) = 1:nrow(networkMatrix)
      }else{
        # if the propertyDf is not Null, the matrix name will use the propertyDf names.
        rownames(networkMatrix) = rownames(propertyDf)
        colnames(networkMatrix) = rownames(propertyDf)
      }
    }else{
      colnames(networkMatrix) = rownames(networkMatrix)
    }
  }

  if(!is.null(rownames(propertyDf))) rownames(propertyDf) = rownames(networkMatrix)

  # transfer the network Matrix to links items.
  networkMatrix[!lower.tri(networkMatrix)] <- NA
  networkMatrix[networkMatrix==0] <- NA
  validNode <- as.data.frame(t(which(!is.na(networkMatrix), arr.ind=TRUE)))
  linksOutput <- lapply(validNode, FUN=function(nodeIndex){
    return(
      list(
        source = nodeIndex[1] - 1,
        target = nodeIndex[2] - 1,
        weight = networkMatrix[nodeIndex[1], nodeIndex[2]]
      )
    )
  })

  names(linksOutput) <- NULL

  # set the nodes property item.

  #set the default color array.
  .gg.color.hue <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
  }

  #If the propertyDf is null, will use category = 0, value=0 as default.
  if (is.null(propertyDf)){
    nodesOutput <- lapply(colnames(networkMatrix), FUN = function(nodeName){
      return(
        list(
          category = 0,
          name = nodeName,
          value = 0
        )
      )
    })

    categoriesOutput <- list(list(
      name = "Default",
      itemStyle = list(
        normal = list(
          color = .gg.color.hue(1)
        )
      )
    ))
    categoryList = list("Default")
  }else{
    if(is.null(propertyDf$value)){
      # if the propertyDf has no column named value, the value will set to 0.
      propertyDf$value=0
    }
    if(is.null(propertyDf$color)){
      # if the propertyDf has no column named color, the color will be default to the .gg.color.hue class.
      # Also, the color will be .gg.color.hue(1) if the category column missed at the same time.
      if (is.null(propertyDf$category)){
        propertyDf$category = 0
        propertyDf$color = .gg.color.hue(1)
      }else{
        categoryList = unique(propertyDf$category)
        colArray = .gg.color.hue(length(categoryList))
        for(category in categoryList ){
          propertyDf[which(propertyDf$category == category), "color"] = colArray[which(categoryList == category)]
        }
      }
    }

    categoryList = unique(propertyDf$category)

    legendData = categoryList

    nodesOutput <- lapply(colnames(networkMatrix), FUN = function(nodeName){
      indexOfDf = which(rownames(propertyDf) == nodeName)[1]
      if(is.na (indexOfDf)){
        return(
          list(
            category = 0,
            name = nodeName,
            value = 0
          )
        )
      }else{
        return(
          list(
            category = which(categoryList == propertyDf[indexOfDf, "category"]) - 1,
            name = nodeName,
            value = propertyDf[indexOfDf, "value"]
          )
        )
      }
    })

    categoriesOutput <- lapply(categoryList, function(category){
      return(
        list(
          name = category,
          itemStyle = list(
            normal = list(
              color = propertyDf[which(propertyDf$category == category),  "color"][1]
            )
          )
        )
      )
    }
    )
  }

  #legendData set


  if(is.null(opt$series$type)) {
    opt$series$type = "force"
  }

  if(is.null(opt$series$minRadius)) {
    opt$series$minRadius = minR
  }

  if(is.null(opt$series$maxRadius)) {
    opt$series$maxRadius = maxR
  }

  if(is.null(opt$series$density)) {
    opt$series$density = density
  }

  if(is.null(opt$series$attractiveness)) {
    opt$series$attractiveness = attractiveness
  }

  if(is.null(opt$series$itemStyle)) {
    itemStyleOutput = list(
      normal = list(
        label = list(
          show = "true",
          textStyle = list(color="#800080")
        ),
        nodeStyle = list(
          brushType = "both",
          strokeColor = "rgba(255,215,0,0.4)",
          lineWidth = 8
        )
      ),
      emphasis = list(
        label = list(
          show = "true"
        ),
        nodeStyle = list(
          r = maxR
        )
      )
    )
  }
  #legend setting
  # option$legend Should set at last...
  # if(legend){
  #	opt$legend = list(
  #		show =  "true",
  #		x = matchPos.x(legend.x),
  #		y = matchPos.y(legend.y),
  #		orient =  match.arg(legend.orient)
  #	)
  #}else{
  #	opt$legend = list(
  #		show = ifelse(legend, "true", "false")
  #	)
  #}

  opt$legend = legendSet( show=legend, data=categoryList, legend.x=legend.x, legend.y=legend.y, orient=legend.orient)


  opt$series$itemStyle = itemStyleOutput
  opt$series$categories = categoriesOutput
  opt$series$nodes = nodesOutput
  opt$series$links = linksOutput
  opt$series = list(opt$series)

  opt$size = size

  ### output list format
  chart = htmlwidgets::createWidget(
    'echarts', opt,
    package = 'recharts', width = size[1], height = size[2],
    preRenderHook = function(instance) {
      instance
    }
  )
  chart = .addClass(chart, "eForce")
  chart
}

eMap = function(chart, ...) {

}

eGauge = function(chart, ...) {

}

#' Create an HTML funnel charts widget using the ECharts(version:3.2.2) library
#'
#' This function creates an HTML widget to display data.frame and
#' factor array, using the JavaScript library ECharts3.
#' @param dat a data object (a data frame or a numeric array with name)
#' @param namevar objects of class "formula" (or one that can be coerced
#'   to that class):  the funnel name of the given data.frame colnames, e.g.
#'   \code{namevar = ~funnelName};  namevar only needed for the
#'   data.frame data input.
#' @param datavar an "formula" object: Associates the value of variable
#'   with symbol color, e.g. \code{datavar = ~valueName}.
#' @param size an array of html widget width and height(either numeric pixels
#'   or percentage could be accepted): e.g. size = c(1024, 768).
#' @param fixed Fixed the max of data into 100%,
#'   default is \code{TRUE}.
#' @param  theme an object of theme name. see(\url{http://datatables.net/extensions/index}) for detail.
#'   supported theme: \code{c("default", "vintage", "dark", "westeros", "essos", "wonderland", "walden",
#'   "chalk", "infographic", "macarons", "roma", "shine", "purple-passion")}
#' @param title an overall title for the plot. you can modify title widget after chart has been
#'   generated: Echart + eTitle(title = "your title.")
#' @param title.x,title.y the xy coordinates of main title, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify title widget after chart has been generated:
#'   \code{Echart + eTitle(title="main title", x = "left", y=10)}
#' @param legend logical whether the legend widget show or not, default is TRUE.
#'   you can modify legend widget after chart has been generated, the legend position and
#'   legend orientation are available at present.
#'   \code{Echart + eLegend(show = TRUE)}
#' @param legend.x,legend.y the xy coordinates of legend, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(x = "right", y="top")}
#' @param legend.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(orient = "vertical")}
#' @param toolbox logical whether the toolbox widget show or not, default is TRUE.
#'   you can modify toolbox widget after chart has been generated, the toolbox position, toolbox
#'   element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(show = TRUE)}
#' @param toolbox.x,toolbox.y the xy coordinates of toolbox, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(x = "right", y="top")}
#' @param toolbox.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(orient = "vertical")}
#' @param dataview,mark,restore,dataZoom,magicType logical variable whether the dataview
#'   , mark, restore, dataZoom or magicType tool in toolbox widget show or not,
#'   default is TRUE. you can modify toolbox widget after chart has been generated,
#'   the toolbox position, toolbox element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(dataView = FALSE)}
#' @param tooltip logical whether the tooltip widget for front-end interactive chart
#'   show or not. default is TRUE. you can modify tooltip widget after chart has been generated,
#'   the tooltip trigger and tooltip formatter is available at present.
#'   \code{Echart + eTooltip(show = FALSE)}
#' @param tooltip.trigger an element of c("axis", "item"), default is "axis" for bar chart.
#'   "axis" option for trigger will show all the information of mouse;
#'   "item" option for tirgger will only show the given item information of mouse.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(trigger = "axis")}
#' @param tooltip.formatter the information formatter for tooltip widget,
#'   default is "<a>:<b><c>" for bar chart.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(formatter = "<a><b>:<c>")}
#' @param calculable logical whether the front-end interactive chart will
#'   support the drag-recalculable feature.
#'   the size and calculable option can be setted after bar chart has been
#'   generated through eOption: \code{Echart + eOption(calculable = TRUE)}
#' @param showLabel logical whether the region name label showed on chart.
#'   default is TRUE, e.g. \code{Echart + eOption(showLabel = TRUE)}
#' @note You are recommended to use lazyPlot function for interactive chart
#'   option set through "shiny" server.
#' @export
#' @examples
#' x = c("Exposure" = 100, "Click" = 80, "Visit" = 60, "Query"=40, "Buy"=20)
#' eFunnel(x) +eTitle(title = "Funnel Plot")
#' funnelDf <- data.frame(namevar = c("Exposure", "Click", "Visit", "Query", "Buy"),
#'    datavar = c(100, 80, 60, 40, 20), stringsAsFactors=FALSE)
#' eFunnel(funnelDf, ~namevar, ~datavar)
#'
eFunnel = function(dat, namevar=NULL, datavar=NULL, size = NULL, fixed = TRUE,
                   theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top", legend.data = NULL,
                   legend = TRUE, legend.x = "left", legend.y= "top", legend.orient="horizontal",
                   toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top",
                   dataView = TRUE, readOnly = FALSE, mark=TRUE, dataZoom=FALSE, magicType=FALSE,
                   tooltip = TRUE, tooltip.trigger="item", formatter="", axis.scale=TRUE,
                   xlab=FALSE, ylab=FALSE, calculable=TRUE, showLabel=TRUE, opt = list())
{
  if(is.vector(dat) || is.array(dat)){
    dat <- as.data.frame(dat)
    datavar <- 1
    dat$namevar <- rownames(dat)
    namevar <- "namevar"
  }else{
    # if the input dat is not data.frame will format it into data.frame.
    if (class(dat) != "data.frame") dat <- as.data.frame(dat)

    # if the user input argument namevar is null, will use colume one as name input
    if(is.null(namevar)){
      namevar <- 1
    }else{
      namevar = autoArgLabel(namevar, deparse(substitute(namevar)))
      # namevar = evalFormula(namevar, data)
    }
    if(is.null(datavar)){
      datavar <- 2
    }else{
      datavar = autoArgLabel(datavar, deparse(substitute(datavar)))
      # datavar = evalFormula(datavar, data)
    }
  }

  if(fixed) dat[[datavar]] = round(dat[[datavar]]/max(dat[[datavar]])*100)

  if(missing(legend.data) | is.null(legend.data)){legendData = dat[[namevar]]
  }else{legendData = legend.data}

  opt$legend = legendSet( show=legend, data=legendData, legend.x=legend.x, legend.y=legend.y, orient=legend.orient)

  # option$title format.
  opt$title = tilteSet(title = title, subtitle=subtitle,
                       title.x = title.x, title.y = title.y)

  opt$calculable = calculableSet(calculable = calculable)
  opt$theme = themeSet(theme = theme)
  # opt$tooltip format, not open to user now.
  opt$tooltip = tooltipSet( tooltip=tooltip,trigger=tooltip.trigger,
                            formatter = "{a} <br/>{b} : {c}%", islandFormatter="")


  datFrame = data.frame(value=dat[[datavar]], name=dat[[namevar]])
  datList = unname(lapply(split(datFrame, seq_len(nrow(datFrame))), as.list))

  #showLabelLine=showLabel
  #now we don't support the multiple graph in one canvas
  opt$series = list(
    list(
      name = "Funnel",
      type = "funnel",
      size= c('80%', '80%'),
      textRotation = c(0, 45, 90, -45),
      textPadding = 0,
      data = datList
    )
  )

  #jsonStr <- toJSON(opt, pretty=TRUE)
  opt$size = size

  ### output list format
  chart = htmlwidgets::createWidget(
    'echarts', opt, width = size[1], height = size[2], package = 'recharts',
    preRenderHook = function(instance) {
      instance
    }
  )
  chart
}

eEventRiver = function(chart, ...) {

}



#' Create an HTML scatter charts widget using the ECharts(version:3.2.2) library
#'
#' This function creates an HTML widget to display data.frame using the JavaScript
#' library ECharts3.
#' @param dat a data object (a matrix, a data frame or a factor array)
#' @param xvar,yvar objects of class "formula" (or one that can be coerced
#'   to that class):  x,y coordinates of the given data.frame colnames, e.g.
#'   \code{xvar = ~xAxisName}; \code{yvar = ~yAxisName}. xvar, yvar only needed for the
#'   data.frame data input.
#' @param series an "formula" object: Associates the levels of variable
#'   with symbol color, e.g. \code{series = ~groupName}
#' @param size an array of html widget width and height(either numeric pixels
#'   or percentage could be accepted): e.g. size = c(1024, 768).
#' @param powers,precision the precision and powers of value display setting.
#'   default: \code{power = 2; precision = 2}
#' @param  theme an object of theme name. see(\url{http://datatables.net/extensions/index}) for detail.
#'   supported theme: \code{c("default", "vintage", "dark", "westeros", "essos", "wonderland", "walden",
#'   "chalk", "infographic", "macarons", "roma", "shine", "purple-passion")}
#' @param title an overall title for the plot. you can modify title widget after chart has been
#'   generated: Echart + eTitle(title = "your title.")
#' @param title.x,title.y the xy coordinates of main title, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify title widget after chart has been generated:
#'   \code{Echart + eTitle(title="main title", x = "left", y=10)}
#' @param legend logical whether the legend widget show or not, default is TRUE.
#'   you can modify legend widget after chart has been generated, the legend position and
#'   legend orientation are available at present.
#'   \code{Echart + eLegend(show = TRUE)}
#' @param legend.x,legend.y the xy coordinates of legend, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(x = "right", y="top")}
#' @param legend.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify legend widget after chart has been generated:
#'   \code{Echart + eLegend(orient = "vertical")}
#' @param toolbox logical whether the toolbox widget show or not, default is TRUE.
#'   you can modify toolbox widget after chart has been generated, the toolbox position, toolbox
#'   element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(show = TRUE)}
#' @param toolbox.x,toolbox.y the xy coordinates of toolbox, besides the excat exact pixels value,
#'   x accept c("left", "right", "center") and y accept c("top", "bottom", "center") as legal input.
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(x = "right", y="top")}
#' @param toolbox.orient an element of c("horizontal", "vertical"), default is "horizontal"
#'   you can modify toolbox widget after chart has been generated:
#'   \code{Echart + eToolbox(orient = "vertical")}
#' @param dataview,mark,restore,dataZoom,magicType logical variable whether the dataview
#'   , mark, restore, dataZoom or magicType tool in toolbox widget show or not,
#'   default is TRUE. you can modify toolbox widget after chart has been generated,
#'   the toolbox position, toolbox element and toolbox orientation are available at present.
#'   \code{Echart + eToolbox(dataView = FALSE)}
#' @param tooltip logical whether the tooltip widget for front-end interactive chart
#'   show or not. default is TRUE. you can modify tooltip widget after chart has been generated,
#'   the tooltip trigger and tooltip formatter is available at present.
#'   \code{Echart + eTooltip(show = FALSE)}
#' @param tooltip.trigger an element of c("axis", "item"), default is "axis" for scatter chart.
#'   "axis" option for trigger will show all the information of mouse;
#'   "item" option for tirgger will only show the given item information of mouse.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(trigger = "axis")}
#' @param tooltip.formatter the information formatter for tooltip widget,
#'   default is "<a>:<b><c>" for scatter chart.
#'   you can modify tooltip widget after chart has been generated:
#'   \code{Echart + eTooltip(formatter = "<a><b>:<c>")}
#' @param calculable logical whether the front-end interactive chart will
#'   support the drag-recalculable feature.
#'   the size and calculable option can be setted after scatter chart has been
#'   generated through eOption: \code{Echart + eOption(calculable = TRUE)}
#' @note You are recommended to use lazyPlot function for interactive chart
#'   option set through "shiny" server.
#' @export
#' @examples
#' ePoints(iris[,3:5], theme=2)
#' iris$Species <- as.character(iris$Species)
#' iris[1:20, "Species"] ="redFlower"
#' ePoints(iris[,3:5], xvar=~Petal.Length, yvar=~Petal.Width, series=~Species, theme=2)
#'


ePoints = function(dat, xvar=NULL, yvar=NULL, series=NULL, size = NULL,   power=2, precision=2,
                   theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top",
                   legend = TRUE, legend.data=NULL, legend.x = "left", legend.y= "top", legend.orient="horizontal",
                   toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top",
                   dataView = TRUE, readOnly = FALSE, mark=TRUE, dataZoom=TRUE, magicType=FALSE,
                   tooltip = TRUE, tooltip.trigger="item", formatter="", axis.scale=TRUE,
                   axis.line=TRUE, axis.tick=FALSE, axis.lable=TRUE, axis.splitLine=TRUE, axis.splitArea=FALSE, axis.boundaryGap=TRUE,
                   xlab=TRUE, xlab.type="value", xlab.data=NULL, xlab.position="bottom",
                   xlab.name = "", xlab.namePosition="start", xlim=NULL,
                   ylab=TRUE, ylab.type="value", ylab.data=NULL, ylab.position="left",
                   ylab.name = "", ylab.namePosition="start", ylim=NULL,
                   calculable=FALSE, showLabel=TRUE, opt = list())
{

  ### ePoint data setting,
  # preprocess data to xvar, yvar, serieslab.
  xvar = autoArgLabel(xvar, deparse(substitute(xvar)))
  yvar = autoArgLabel(yvar, deparse(substitute(yvar)))
  namevar = autoArgLabel(series, deparse(substitute(series)))

  if(missing(xvar) | is.null(xvar) | xvar=="") xvar = colnames(dat)[1]
  if(missing(yvar) | is.null(yvar) | yvar=="" ) yvar = colnames(dat)[2]
  if(missing(namevar) | is.null(namevar)| namevar==""){
    namevar = "defaultName"
    dat[,"defaultName"] = "default"
  }
  dat <- dat[,c(xvar, yvar, namevar)]

  # format the dat to data.frame
  if (class(dat) != "data.frame") dat <- as.data.frame(dat, stringsAsFactor=F)

  if(ncol(dat)<2) {
    stop("dim(dat)[2] should be 2 or 3.")
  }else if(ncol(dat) == 2){
    dat[,3] == "default"
    colnames(dat)[3] = "name"
    namevar = "name"
  }

  if(length(xvar) > 1) xvar = xvar[1]
  if(class(xvar) == "integer" | class(xvar) == "numeric"){
    if (xvar > ncol(dat)){
      stop("wrong xvar input...")
    }else{
      xvar = colnames(dat)[xvar]
    }
  }else if(!xvar %in% colnames(dat)){
    stop("wrong xvar input...")
  }

  if(length(yvar) > 1) yvar = yvar[1]
  if(class(yvar) == "integer" | class(yvar) == "numeric"){
    if (yvar > ncol(dat)){
      stop("wrong yvar input...")
    }else{
      yvar = colnames(dat)[yvar]
    }
  }else if(!yvar %in% colnames(dat)){
    stop("wrong yvar input...")
  }

  if(length(namevar) > 1) namevar = namevar[1]
  if(class(namevar) == "integer" | class(namevar) == "numeric"){
    if (namevar > ncol(dat)){
      stop("wrong namevar input...")
    }else{
      namevar = colnames(dat)[namevar]
    }
  }else if(!namevar %in% colnames(dat)){
    stop("wrong namevar input...")
  }

  # if the xvar/yvar/namevar is null, will use the first column of dat as default.	And check the xvar in the dat colnames.
  if (is.null(xvar) || !xvar %in% colnames(dat)) xvar = colnames(dat)[1]
  if (is.null(yvar) || !yvar %in% colnames(dat)) yvar = colnames(dat)[2]
  if (is.null(namevar) || !namevar %in% colnames(dat)) namevar = colnames(dat)[3]

  dat <- dat[,c(xvar, yvar, namevar)]

  group <- unique(dat[,3])

  # option$title format.
  opt$title = tilteSet(title = title, subtitle=subtitle,
                       title.x = title.x, title.y = title.y)

  opt$calculable = calculableSet(calculable = calculable)
  opt$theme = themeSet(theme = theme)

  # opt$tooltip format, not open to user now.
  opt$tooltip = tooltipSet( tooltip=tooltip,trigger=tooltip.trigger,
                            formatter = "", islandFormatter="")

  opt$toolbox = toolboxSet(toolbox=toolbox, toolbox.x=toolbox.x, toolbox.y=toolbox.y, orient=toolbox.orient,
                           dataView=dataView, mark=mark, dataZoom = dataZoom, magicType = magicType, restore = TRUE, readOnly = readOnly,
                           saveAsImage=TRUE)

  if(missing(legend.data) | is.null(legend.data)){legendData = group
  }else{legendData = legend.data}

  opt$legend = legendSet( show=legend, data=legendData, legend.x=legend.x, legend.y=legend.y, orient=legend.orient)

  opt$xAxis = xAxisSet(axisShow=xlab, type=xlab.type, data=xlab.data, position=xlab.position,
                       labelName=xlab.name, label.namePosition=xlab.namePosition, lim=xlim,
                       axisLine=axis.line, axisTick=axis.tick, axisLable=axis.lable, splitLine=axis.splitLine,
                       splitArea=axis.splitArea, boundaryGap=axis.boundaryGap, scale=axis.scale, power=power, precision=precision)

  opt$yAxis = yAxisSet(axisShow=ylab, type=ylab.type, data=ylab.data, position=ylab.position,
                       labelName=ylab.name, label.namePosition=ylab.namePosition, lim=ylim,
                       axisLine=axis.line, axisTick=axis.tick, axisLable=axis.lable, splitLine=axis.splitLine,
                       splitArea=axis.splitArea, boundaryGap=axis.boundaryGap, scale=axis.scale, power=power, precision=precision)


  opt$series =  vector("list", length(group))
  for(i in 1:length(group)) {
    if(dim(dat)[2]==2) {
      mat = as.matrix(dat)
    } else {
      mat = as.matrix(dat[which(dat[,3]==group[i]), 1:2])
    }
    colnames(mat) = NULL
    rownames(mat) = NULL

    if(is.null(opt$series[[i]]$type)) {
      opt$series[[i]]$type = 'scatter'
    }

    if(is.null(opt$series[[i]]$name) & dim(dat)[2]==3) {
      opt$series[[i]]$name = group[i]
    } else {
      warning("You'd better set series:name with y.")
    }

    if(is.null(opt$series[[i]]$data)) {
      opt$series[[i]]$data = mat
    } else {
      warning('You can set series:data with y.')
    }
  }

  #jsonStr <- toJSON(opt, pretty=TRUE)
  #outList <- .rechartsOutput(jsonStr, charttype="ePoints", size=size)
  opt$size = size

  ### output list format
  chart = htmlwidgets::createWidget(
    'echarts', opt,
    package = 'recharts', width = size[1], height = size[2],
    preRenderHook = function(instance) {
      instance
    }
  )
  chart = .addClass(chart, "ePoints")
  chart
}

#' @export
eScatter = ePoints
