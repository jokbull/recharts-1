
#' @export
ec <- function(data = NULL, x = NULL, y = NULL, series = NULL, type = 'auto',
width = NULL, height = NULL, ...
) {

  settings = list(...)

  #
  type = tolower(type)

  if (type == 'auto') {
    stop("Sorry, not support auto temporarily.")
    #type = determineType(x, y)
    return(invisible(NULL))
  } else if (type == "line" | type == "l") {
    print(paste0("YES, I KNOW THIS TYPE:",type))
  } else if (type == "k") {
    print(paste0("YES, I KNOW THIS TYPE:",type))
  } else {
    stop("Sorry, not support ", type, "temporarily.")
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
    dependencies = getDependency(NULL),
    preRenderHook = function(instance) {
      instance
    }
  )
  class(chart) <- c(class(chart), "echarts" )

  if (hasArg("options")) {
    chart = Reduce(setOptionsFromJson, x = settings$options ,init = chart)
  }
  chart
}
