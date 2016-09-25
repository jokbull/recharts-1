
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




