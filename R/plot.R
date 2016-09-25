#' Modify a recharts by adding on new components.
#'
#' @param e1 An object of class \code{recharts}
#' @param e2 A component to add to \code{e1}
#'
#' @export
#'
#' @seealso \code{\link{set}}
#' @method + echarts
"+.echarts" <- function(e1, e2){
  if (is.vector(e2)) {
    return(addSeries(e1,e2))
  } else if (!is.character(e2)) {
    e2name <- deparse(substitute(e2))
  } else {
    e2name <- e2
  }

  e2name <- stringr::str_trim(e2name)
  functionName = gsub("\\(.*", "", e2name)
  fun = getFromNamespace(functionName,'recharts')
  params = stringr::str_match(e2name, "\\((.*)\\)")[2]
  setFuncList <-
    c( "eTitle", "eToolbox", "eCalculable",
       "eLegend", "eTooltip", "eDataRange",
       "eXAxis", "eYAxis", "ePolar", "eDataZoom",
       "eTheme")
  if (!functionName %in% setFuncList){
    stop(paste("unspported eCharts setting function inputs", functionName))
    return(NULL)
  } else {
    functionName = paste0(tolower(substr(functionName,2,2)), substr(functionName,3,100))
  }

  if ("echarts" %in% class(e1)) {
    setOptions(e1,eval(parse(text=
                               sprintf("list(%s=list(%s))",functionName,params)
    )))
  }
}

#' @export
"%+%" <- `+.echarts`






#' Merge the two ECharts into one output .
#'
#' @param e1 An object of class \code{recharts}
#' @param e2 An object of class \code{recharts}
#'
#' @export
#'
#' @seealso \code{\link{set}}
#' @method & echarts
"&.echarts" <- function(e1, e2){
  if(!(inherits(e1, "echarts") & inherits(e2, "echarts")))
    stop("only echarts object can be merged into one widgets...")

  chart = htmlwidgets::appendContent(e1, e2)
  class(chart)[3] = "multi-echarts"
  return(chart)
}


#' @export
"%&%" <- `&.echarts`









#' Set recharts title option
#'
#' @export
#'
eTitle = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}
#' Set recharts general option
#'
#' @export
#'
eOption = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}
#' Set recharts toolbox option
#'
#' @export
#'
eToolbox = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}
#' Set recharts drag-recaluculation option
#'
#' @export
#'
eCalculable = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}
#' Set recharts legend option
#'
#' @export
#'
eLegend = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}
#' Set recharts tooltip option
#'
#' @export
#'
eTooltip = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}
#' Set recharts dataRange option
#'
#' @export
#'
eDataRange = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}

#' Set recharts x Axis option
#'
#' @export
#'
eXAxis = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}

#' Set recharts y Axis option
#'
#' @export
#'
eYAxis = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}
#' Set recharts polar option
#'
#' @export
#'
ePolar = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}
#' Set recharts x dataZoom option
#'
#' @export
#'
eDataZoom = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}
#' Set recharts theme option
#'
#' @export
#'
eTheme = function(...){
  elements <- list(...)
  structure(elements, class ="option")
}



