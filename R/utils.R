# make sure htmlwidgets:::toJSON() turns list() to {} in JSON, instead of []
.emptyList = setNames(list(), character())
emptyList = function() .emptyList

# evaluate a formula using `data` as the environment, e.g. evalFormula(~ z + 1,
# data = data.frame(z = 1:10))
evalFormula = function(x, data) {
  if (!inherits(x, 'formula')) return(x)
  if (length(x) != 2) stop('The formula must be one-sided: ', deparse(x))
  eval(x[[2]], data, environment(x))
}

# merge two lists by names, e.g. x = list(a = 1, b = 2), mergeList(x, list(b =
# 3)) => list(a = 1, b = 3)
mergeList = function(x, y) {
  if (!is.list(y) || length(y) == 0) return(x)
  yn = names(y)
  if (length(yn) == 0 || any(yn == '')) {
    warning('The second list to be merged into the first must be named')
    return(x)
  }
  for (i in yn) {
    xi = x[[i]]
    yi = y[[i]]
    if (is.list(xi)) {
      if (is.list(yi)) x[[i]] = mergeList(xi, yi)
    } else x[[i]] = yi
  }
  x
}

# automatic labels from function arguments
autoArgLabel = function(arg, auto) {
  if (is.null(arg)) return('')
  if (inherits(arg, 'formula')) return(deparse(arg[[2]]))
  auto
}

strstrip <- function(string, side = c("both", "left", "right")) {
  side <- match.arg(side)
  pattern <- switch(side, left = "^\\s+", right = "\\s+$", both = "^\\s+|\\s+$")
  OUT <- gsub(pattern, "", string)
  return(OUT)
}


matchPos.x <- function(x){
  X <- tryCatch({
    x <- as.numeric(x)
    x <- ifelse(is.na(x), "", x)
    return(x)
  },warning = function(w){
    match.arg(x,c("center", "left", "right"))
  })
  return(X)
}

matchPos.y <- function(y){
  Y <- tryCatch({
    y <- as.numeric(y)
    y <- ifelse(is.na(y), "", y)
    return(y)
  },warning = function(w){
    match.arg(y,c("bottom", "center", "top"))
  })
  return(Y)
}

.addClass <- function(chart, className){
  class(chart) <- c(class(chart), className )
  return(chart)
}

unnames = function(x){
  names(x) = NULL
  return(x)
}




regJson <- function(txt,...) {
  txt = stringr::str_replace_all(txt,"(\\w+)(:)","\"\\1\"\\2")
  txt = stringr::str_replace_all(txt,"\'","\"")
  if (is.na(stringr::str_match(txt, "^\\{"))){
    txt = paste0("{",txt, "}")
  }
  txt
}








