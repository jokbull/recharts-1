#' @export
#' @example
#' mpdata = list(list(coord=c("2016-01-12",10)), list(coord=c("2016-01-17",15)))
#' addMarkPoint(k1,mpdata)
#' mpdf1 = data.frame(x=c("2016-01-02","2016-01-20"),y=c(30,30), color=c("steelblue","orange"), symbol="pin", symbolRotate=c(0,180), ...)
#' addMarkPoint(k1,mpdf1)
addMarkPoint = function(chart, mpdata, series = 1, ...) {
  newop = list(...)
  if (is.character(mpdata)) {
    data = jsonlite::fromJSON(regJson(mpdata))
  } else if (is.data.frame(mpdata)) {
    if (ncol(mpdata)>=2) {
      data = df2list(mpdata)
    } else {
      stop("Not Support MarkPoints Type. Data.frame with 2-columns")
    }
  } else {
    data = mpdata
  }


  newop$data = data
  if ("data" %in% names(chart$x$series)) {
    if (is.null(chart$x$series$markPoint)) {
      chart$x$series$markPoint = newop
    } else {
      chart$x$series$markPoint = rlist::list.merge(chart$x$series$markPoint, newop)
      chart$x$series$markPoint$data = c(chart$x$series$markPoint$data, data)
    }
  } else {
    if (series > length(chart$x$series) | series < 1) {
      series = 1
    }
    if (is.null(chart$x$series[[series]]$markPoint)) {
      chart$x$series[[series]]$markPoint = newop
    } else {
      chart$x$series[[series]]$markPoint = rlist::list.merge(chart$x$series[[series]]$markPoint, newop)
      chart$x$series[[series]]$markPoint$data = c(chart$x$series[[series]]$markPoint$data, data)

    }
  }
  chart
}
#' @export
clearMarkPoint = function(chart, series = 1) {
  if ("data" %in% names(chart$x$series)) {
    chart$x$series$markPoint = NULL
  } else {
    chart$x$series[[series]]$markPoint = NULL
  }
  chart
}


df2list = function(df) {
  res = list()
  columnNames = colnames(mpdf1)
  for (i in seq_len(nrow(df))) {
    tmplst = list(coord = c(as.character(df[i,"x"]),as.character(df[i,"y"])))
    if ('color' %in% columnNames) {
      tmplst$itemStyle = list(normal=list(color = as.character(df[i,"color"])),
                              emphasis = list(color = as.character(df[i,"color"])))
    }
    if ('symbol' %in% columnNames) {
      tmplst$symbol = as.character(df[i,"symbol"])
    }
    if ('symbolRotate' %in% columnNames) {
      tmplst$symbolRotate = as.character(df[i,"symbolRotate"])
    }

    res = c(res, list(tmplst))
  }
  res
}



