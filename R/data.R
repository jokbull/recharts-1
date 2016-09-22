
data_pie = function(x, y, series = NULL, type = 'pie') {
  if (is.null(series)) {
    xy=data.frame(name=x,value=y)
  } else {
    xy=data.frame(name=series,value=y)
  }
  xy$name=as.character(xy$name)
  data1=list()
  for (i in 1:dim(xy)[1]){
    data1[[i]]=list(name=xy[i,1],value=xy[i,2])
  }
  return(list(list(type = type,
                   showScale = TRUE,
                   showScaleText = TRUE,
                   data = data1
  )))
}



# split the data matrix for a scatterplot by series
data_scatter = function(x, y, series = NULL, type = 'scatter') {
  xy = unname(cbind(x, y))
  if (is.null(series)) return(list(list(type = type, data = xy)))
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(as.matrix(xy[[i]])))
  }
  obj
}

data_bar = function(x, y, series = NULL, type = 'bar') {

  # plot the frequencies of x when y is not provided
  if (is.null(y)) {

    if (is.null(series)) {
      y = table(x)
      return(list(list(type = type, data = unname(c(y)))))
    }

    y = table(x, series)
    nms = colnames(y)
    obj = list()
    for (i in seq_len(ncol(y))) {
      obj[[i]] = list(name = nms[i], type = type, data = unname(y[, i]))
    }
    return(obj)

  }

  # when y is provided, use y as the height of bars
  if (is.null(series)) {
    return(list(list(type = type, data = y)))
  }

  xy = tapply(y, list(x, series), function(z) {
    if (length(z) == 1) return(z)
    stop('y must only have one value corresponding to each combination of x and series')
  })
  nms = colnames(xy)
  obj = list()
  for (i in seq_len(ncol(xy))) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(xy[, i]))
  }
  obj

}


data_K = function(x, y, series=NULL){
  # only support 1-serie K chart
  # y should be a N*4 matrix or data.frame
  obj = list()
  obj[[1]] = list(name = 'K', type = 'k', data = unname(as.matrix(y)))
  obj
}
