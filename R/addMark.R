#' @title Mark Point
#' @name add/clear MarkPoints
#' @export
#' @examples
#' mpdata = list(list(coord=c("2016-01-12",10)), list(coord=c("2016-01-17",15)))
#' addMarkPoint(k1,mpdata)
#' mpdf1 = data.frame(x=c("2016-01-02","2016-01-20"),y=c(30,30), color=c("steelblue","orange"), symbol="pin", symbolRotate=c(0,180), ...)
#' addMarkPoint(k1,mpdf1)
#' addMarkPoint(k1) <=> clearMarkPoint
addMarkPoint = function(chart, mpdata, series = 1, ...) {
  if (missing(mpdata) || is.null(mpdata)) {
    clearMarkPoint(chart, series)
    return
  }
  newop = list(...)
  if (is.character(mpdata)) {
    data = jsonlite::fromJSON(regJson(mpdata))
  } else if (is.data.frame(mpdata)) {
    if (ncol(mpdata)>=2) {
      data = df2list_markPoints(mpdata)
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


#' @title Mark Line
#' @name add/clear MarkLine
#' @export
#' @examples
#' mpdata = list(list(coord=c("2016-01-12",10)), list(coord=c("2016-01-17",15)))
#' addMarkPoint(k1,mpdata)
#' mldf1 = data.frame(x0=c("2016-01-02","2016-01-17"),y0=c(30,30), x=c("2016-01-05","2016-01-20"),y=c(20,40), color=c("steelblue","orange"), symbol="pin", symbolRotate=c(0,180), width=1,type="solid")
#' addMarkPoint(k1,mpdf1)
addMarkLine = function(chart, mpdata, series = 1, ...) {
  if (missing(mpdata) || is.null(mpdata)) {
    clearMarkLine(chart, series)
    return
  }
  newop = list(...)
  if (is.character(mpdata)) {
    data = jsonlite::fromJSON(regJson(mpdata))
  } else if (is.data.frame(mpdata)) {
    if (ncol(mpdata)>=2) {
      data = df2list_markLines(mpdata)
    } else {
      stop("Not Support MarkLines Type. Data.frame with 2-columns")
    }
  } else {
    data = mpdata
  }


  newop$data = data
  if ("data" %in% names(chart$x$series)) {
    if (is.null(chart$x$series$markLine)) {
      chart$x$series$markLine = newop
    } else {
      chart$x$series$markLine = rlist::list.merge(chart$x$series$markLine, newop)
      chart$x$series$markLine$data = c(chart$x$series$markLine$data, data)
    }
  } else {
    if (series > length(chart$x$series) | series < 1) {
      series = 1
    }
    if (is.null(chart$x$series[[series]]$markLine)) {
      chart$x$series[[series]]$markLine = newop
    } else {
      chart$x$series[[series]]$markLine = rlist::list.merge(chart$x$series[[series]]$markLine, newop)
      chart$x$series[[series]]$markLine$data = c(chart$x$series[[series]]$markLine$data, data)

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

#' @export
clearMarkLine = function(chart, series = 1) {
  if ("data" %in% names(chart$x$series)) {
    chart$x$series$markLine = NULL
  } else {
    chart$x$series[[series]]$markLine = NULL
  }
  chart
}



df2list_markPoints = function(df) {
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



df2list_markLines = function(df) {
  res = list()
  columnNames = colnames(df)
  for (i in seq_len(nrow(df))) {
    tmplst = list()
    tmplst = list(
      "0" = list(coord=c(as.character(df[i,"x0"]),as.character(df[i,"y0"]))),
      "1" = list(coord=c(as.character(df[i,"x"]),as.character(df[i,"y"])),value = as.character(df[i,"y"]))
    )

    if ('color' %in% columnNames) {
      tmplst[[1]]$lineStyle$normal$color = as.character(df[i,"color"])
      tmplst[[1]]$lineStyle$emphasis$color = as.character(df[i,"color"])
    }
    if ('width' %in% columnNames) {
      tmplst[[1]]$lineStyle$normal$width = as.character(df[i,"width"])
      tmplst[[1]]$lineStyle$emphasis$width = as.character(df[i,"width"])
    }
    if ('type' %in% columnNames) {
      tmplst[[1]]$lineStyle$normal$type = as.character(df[i,"type"])
      tmplst[[1]]$lineStyle$emphasis$type = as.character(df[i,"type"])
    }
    if ('symbol' %in% columnNames) {
      tmplst[[1]]$symbol = as.character(df[i,"symbol"])
    }
    if ('symbolSize' %in% columnNames) {
      tmplst[[1]]$symbolSize = as.character(df[i,"symbolSize"])
    }
    if ('symbolRotate' %in% columnNames) {
      tmplst[[1]]$symbolRotate = as.character(df[i,"symbolRotate"])
    }
    tmplst[[1]]$label$normal = list(show=TRUE)
    tmplst[[1]]$label$emphasis= list(show=TRUE)

    res = c(res, list(tmplst))
  }
  res
}



