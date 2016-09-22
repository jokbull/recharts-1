

adjust_common = function(chart, ...) {
  if (hasArg(theme)) {
    chart = eThemeSet(chart,...)
  }
  chart = eTooltipSet(chart, ...)
  chart = eToolboxSet(chart, ...)
  chart
}

adjust_line = function(chart, ...) {
  chart
}
