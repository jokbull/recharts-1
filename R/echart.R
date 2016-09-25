#' Create an ECharts widget
#'
#' Create an HTML widget for ECharts that can be rendered in the R console, R
#' Markdown documents, or Shiny apps. You can add more components to this widget
#' and customize options later. \code{eChart()} is an alias of \code{echart()}.
#' @param data a data object (usually a data frame or a list)
#' @rdname eChart
#' @examples library(recharts)
#' echart(iris, ~ Sepal.Length, ~ Sepal.Width)
#' echart(iris, ~ Sepal.Length, ~ Sepal.Width, series = ~ Species)
echart = function(data, ...) {
  UseMethod('echart')
}


#' @rdname eChart
echart.list = function(data, width = NULL, height = NULL, ...) {
  htmlwidgets::createWidget(
    'echarts', data, width = width, height = height, package = 'recharts'
  )
}

#' @param x the x variable
#' @param y the y variable
#' @rdname eChart
echart.data.frame = function(
  data = NULL, x = NULL, y = NULL, series = NULL, type = 'auto',
  width = NULL, height = NULL, theme = c("macarons","infographic","dark","roma","shine","vintage"),
  dependency = NULL, ...
) {


  settings = list(...)
  theme = match.arg(theme)
  #
  type = tolower(type)

  if (type == 'auto') {
    stop("Sorry, not support auto temporarily.")
    #type = determineType(x, y)
    return(invisible(NULL))
  } else if (type == "line") {
    print(paste0("YES, I KNOW THIS TYPE:",type))
  } else if (type == "k") {
    print(paste0("YES, I KNOW THIS TYPE:",type))
  } else if (type == "scatter") {
    print(paste0("YES, I KNOW THIS TYPE:",type))
  } else if (type == "bar") {
    print(paste0("YES, I KNOW THIS TYPE:",type))
  } else {
    print("Sorry, not support ", type, "temporarily.")
    return(invisible(NULL))
  }

  param_fun = getFromNamespace(paste0('param_', type), 'recharts')
  params = param_fun(data, x, y, series, ...)
  params$size = c(width, height)

  #   if (!is.null(series)) {
  #     params$legend = list(data = levels(as.factor(series)))
  #   }

  chart = htmlwidgets::createWidget(
    'echarts', params, width = width, height = height, package = 'recharts',
    #dependencies = getDependency(dependency),
    dependencies = getTheme(theme),
    preRenderHook = function(instance) {
      instance
    }
  )
  class(chart) <- c(class(chart), "echarts" )

  if (hasArg("options")) {
    chart = Reduce(setOptionsFromJson, x = settings$options ,init = chart)
  }

  defaultSetting_fun = getFromNamespace(paste0('defaultSetting_', type), 'recharts')
  defaultSetting_fun(chart)
}


#' @rdname eChart
echart.default = echart.data.frame


#' @rdname eChart
eChart = echart
# from the planet of "Duo1 Qiao1 Yi1 Ge4 Jian4 Hui4 Si3" (will die if having to
# press one more key, i.e. Shift in this case)

determineType = function(x, y) {
  if (is.numeric(x) && is.numeric(y)) return('scatter')
  # when y is numeric, plot y against x; when y is NULL, treat x as a
  # categorical variable, and plot its frequencies
  if ((is.factor(x) || is.character(x)) && (is.numeric(y) || is.null(y)))
    return('bar')
  if (is.null(x) && is.ts(y)) return('line')
  # FIXME: 'histogram' is not a standard plot type of ECharts
  # http://echarts.baidu.com/doc/doc.html
  if (is.numeric(x) && is.null(y)) return('histogram')
  message('The structure of x:')
  str(x)
  message('The structure of y:')
  str(y)
  stop('Unable to determine the chart type from x and y automatically')
}

# not usable yet; see https://github.com/ecomfe/echarts/issues/1065
getDependency = function(type) {
  if (is.null(type)) return()
  htmltools::htmlDependency(
    'echarts-module', EChartsVersion,
    src = system.file('htmlwidgets/lib/echarts/chart', package = 'recharts'),
    script = sprintf('%s.js', type)
  )
}

getTheme = function(theme) {
  htmltools::htmlDependency(
    'echarts-module', EChartsVersion,
    src = system.file('htmlwidgets/lib/echarts/theme', package = 'recharts'),
    script = sprintf('%s.js', theme)
  )
}



getMeta = function(chart) {
  attr(chart$x, 'meta', exact = TRUE)
}


