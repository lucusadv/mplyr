
#' Obtains an array from a data frame
#' 
#' @param X data frame
#' @param id_var field 
#' @param ... names of columns across which to aggregate
#' @return an array
#' @export
#' @examples df2c(iris, "Sepal.Length", "Species", "Petal.Width")
#'           df2c(iris, Sepal.Length, Species, Petal.Width)
df2c <- function(data, FUN, id_var, ...){
  id_var <- as.character(eval(substitute(alist(id_var))))
  axes <- as.character(eval(substitute(alist(...))))
  v <- data[, id_var]
  w <- lapply(axes, function(x) data[, x])
  Y <- tapply(v, w, FUN, na.rm=TRUE)  
  Y_names <- dimnames(Y)
  names(Y_names) <- axes
  dimnames(Y) <- Y_names
  class(Y) <- c('array')
  Y
}

#' replacement function that sets axes names for a array. The dimensions 
#' of the array must have names
#'   
#' @param value character vector, names of the exes   
#' @return object with changed axes names
#' @examples A <- array(1:10, dim=c(2,5), dimnames=list(letters[1:2], letters[11:15]))
#'           axes(A) <- c('height','width') 
#' @export
`axes<-` <- function(x, value){ 
  a <- dimnames(x)
  stopifnot(length(a) == length(value))
  names(a) <- value
  dimnames(x) <- a
  return(x)
}

#' function that sets axes names, to be used in a pipe
#'   
#' @param value character vector, names of the exes   
#' @return object with changed axes names
#' @export
set_axes <- function(x, value){
  a <- dimnames(x)
  stopifnot(length(a) == length(value))
  names(a) <- value
  dimnames(x) <- a
  return(x)
}


#' gets axes names
#' @param X array
#' @return character vector
#' @export
axes <- function(X) names(dimnames(X))
 
#' selects a subset of a array in a functional manner
#' 
#' @param X array
#' @param ... logical conditions
#' @return a array
#' @export
#' @examples 
#' X <- array(1:20, dim=c(4,5), dimnames=list(height=letters[1:4], width=letters[11:15]))
#' filter_array(X, height=height >= 'c')
#' filter_array(Titanic, Age=Age=='Child', Class=Class %in% c('2nd','1st'))
filter_array <- function(X, ...){ 
  .dimnames <- dimnames(X)
  .dots <- lazy_dots(...)
  dim_list <- X %>% 
    dim %>% 
    length %>% 
    {rep(T, .)} %>% 
    as.list %>%
  set_names(names(.dimnames))
dim_list_override <- lapply(.dots, function(x) lazy_eval(x, .dimnames))
for (n in names(dim_list_override))
  dim_list[[n]] <- dim_list_override[[n]]
y <- do.call(`[`, c(list(X), dim_list, drop=FALSE))
class(y) <- class(X)
return(y)
}   
 
#' Aggregates the individual labels of one or more axes in order to summarize the cell contents
#'
#' @param X array
#' @param ..., one or more functions of the form <axisname>= <expression of axisnames>
#' @return an Array
#' @export 
groupby_array <- function(X, ...){
  .dimnames <- dimnames(X)
  .dots <- lazy_dots(...)
  dim_list <- X %>% 
    dim %>% 
    length %>% 
    {rep(T, .)} %>% 
    as.list %>%
    set_names(names(.dimnames))
  attributes(X)$grouped_axes <- lapply(.dots, function(x) lazy_eval(x, .dimnames))
  return(X)
}
 
 

#' Aggregates an array across the same values of a dimension
#' 
#' @param X array
#' @param FUN function to apply to all elements in the array with same labels
#' across all axes. In order to obtain reliable results, it is strongly recommended
#' that the function output be independent of the permutation of its arguments
#' @param ... additional parameters to pass to FUN
#' @export
#' @examples
#' R <- getTotrets()
#' axes(R) <- c('date','id')
#' X <- cuaxismutate(R, da
#' te=function(x)substr(x, 1,7))
#' X2 <- aggregate(X, mean)
aggregate_array <- function(X, FUN, ...) {  
  axis_groups <- attributes(X)$grouped_axes
  if (is.null(axis_groups)) {
    stop('Array must be grouped first.')
  }
  for (axis_name in names(axis_groups)){
    dimnames(X)[[axis_name]] <- axis_groups[[axis_name]]
  }
  tmp <- melt(X, as.is=TRUE) 
  tmp2 <- lapply(axes(X), function(n) tmp[, n])
  Y <- tapply(tmp$value, tmp2, FUN, ...)
  axes(Y) <- axes(X)
  return(Y)
}
 

#' align a list of arrays
#'
#' @param X list of arrays. Arrays must have the same number of dimensions (up to 5) but not the same size
#' @param all.dim vector of booleans of equal length as the number of dimensions of the arrays 
#' @param na.value function, value assigned to missing elements
#' @return list of matrices
#' 
#' @author G.A.Paleologo  
#' @export
align_array <- function(X, all.dim = NULL, na.value = NA){
  X_dims <- sapply(X, function(x) length(dim(x)))
  n_dims <- X_dims[1]
  stopifnot (n_dims <= 5)
  stopifnot(length(all.dim)==n_dims)
  X_dimnames <- lapply(X, dimnames)
  X_axisnames <- lapply(X, axes)
  stopifnot(all_identical(X_axisnames))
  opdims <- lapply(all.dim, function(x) if(x) union else intersect)   
  Y_dimnames <- list()
  for (i in seq(opdims)){
    Y_dimnames[[i]] <- Reduce(opdims[[i]], lapply(X_dimnames, '[[', i))
  }
  Y_dimnames <- lapply(Y_dimnames, sort)
  Y_dim <- sapply(Y_dimnames, length)
  Y <- array(na.value, dim = Y_dim, dimnames = Y_dimnames)
  temp <- Y
  out <- list()
  for (n in seq(X)){
    ind <- lapply(seq(n_dims), function(m) intersect(Y_dimnames[[m]], X_dimnames[[n]][[m]]))
    if (n_dims == 2){
      temp[,] <- Y[,]
      temp[ind[[1]], ind[[2]]] <- 
        X[[n]][ind[[1]], ind[[2]], drop=FALSE]
    } else if (n_dims == 3){
      temp[,,] <- Y[,,]
      temp[ind[[1]], ind[[2]], ind[[3]]] <- 
        X[[n]][ind[[1]], ind[[2]], ind[[3]], drop=FALSE]
    } else if (n_dims == 4){
      temp[,,,] <- Y[,,,]
      temp[ind[[1]], ind[[2]], ind[[3]], ind[[4]]] <- 
        X[[n]][ind[[1]], ind[[2]], ind[[3]], ind[[4]], drop=FALSE]
    } else {
      temp[,,,,] <- Y[,,,,]
      temp[ind[[1]], ind[[2]], ind[[3]], ind[[4]], ind[[5]]] <- 
        X[[n]][ind[[1]], ind[[2]], ind[[3]], ind[[4]], ind[[5]], drop=FALSE]
    } 
    out[[n]] <- temp
    axes(out[[n]]) <- X_axisnames[[n]]
  }
  names(out) <- names(X)
  out
}


#' Overlays a array on another
#' 
#' Given two arrays, the function: i) outer aligns them; replaces a NA value in the first one with the 
#' corresponding element of the second
#'
#' @param X array
#' @param Y array
#' @return array
#' 
#' @export
overlay <- function(X, Y){
  X_n <- length(dim(X))
  Y_n <- length(dim(Y))
  if (X_n != Y_n) stop('The two arrays must have the same number of dimensions.')
  tmp <- align_array(list(X, Y), all.dim=rep(TRUE, X_n))
  OUT <- ifelse(is.na(tmp[[1]]), tmp[[2]], tmp[[1]])
  OUT
}


#' array accumulator
#'
#' @param X list of arrays. Arrays must have the same number of dimensions (up to 5) but not the same size
#' @param all.dim vector of booleans of equal length as the number of dimensions of the arrays
#' @param FUN function, reduction function applied element-wise to the matrices
#' @param na.value function, value assigned to missing elements
#' @param ... additional arguments given to FUN
#' @return an array
#' @author G.A.Paleologo  
#' @export
accumulate <- function(X, all.dim = NULL, FUN =`+`, na.value = 0, ...){
  X_axisnames <- lapply(X, axes)
  stopifnot(all_identical(X_axisnames))
  X_dims <- sapply(X, function(x) length(dim(x)))
  n_dims <- X_dims[1]
  stopifnot (n_dims <= 5)
  stopifnot(length(all.dim)==n_dims)
  X_dimnames <- lapply(X, dimnames)
  opdims <- lapply(all.dim, function(x) if(x) union else intersect)    
  Y_dimnames <- list()
  for (i in seq(opdims)){
    Y_dimnames[[i]] <- Reduce(opdims[[i]], lapply(X_dimnames, '[[', i))
  }
  Y_dimnames <- lapply(Y_dimnames, sort)
  Y_dim <- sapply(Y_dimnames, length)
  Y <- array(na.value, dim = Y_dim, dimnames = Y_dimnames)
  out <- temp <- Y
  for (n in seq(X)){
    ind <- lapply(seq(n_dims), function(m) intersect(Y_dimnames[[m]], X_dimnames[[n]][[m]]))
    #   
    if (n_dims == 2){
      temp[,] <- Y[,]
      temp[ind[[1]], ind[[2]]] <- X[[n]][ind[[1]], ind[[2]]]
      out <- FUN(out, temp, ...)
    } else if (n_dims == 3){
      temp[,,] <- Y[,,]
      temp[ind[[1]], ind[[2]], ind[[3]]] <- X[[n]][ind[[1]], ind[[2]], ind[[3]]]
      out <- FUN(out, temp, ...)
    } else if (n_dims == 4){
      temp[,,,] <- Y[,,,]
      temp[ind[[1]], ind[[2]], ind[[3]], ind[[4]]] <- X[[n]][ind[[1]], ind[[2]], ind[[3]], ind[[4]]]
      out <- FUN(out, temp, ...)
    } else {
      temp[,,,,] <- Y[,,,,]
      temp[ind[[1]], ind[[2]], ind[[3]], ind[[4]], ind[[5]]] <- X[[n]][ind[[1]], ind[[2]], ind[[3]], ind[[4]], ind[[5]]]
      out <- FUN(out, temp, ...)
    } 
  }
  if (!is.null(X_axisnames[[1]])) axes(out) <- X_axisnames[[1]]
  out
}

#' outer joins of two or more n-way arrays, checking they have no dimension names in common
#'
#' @param X a list of arrays. Arrays must be numeric, integer or logical
#' and have the same number of dimensions (up to 5) but not necessarily the same
#' size and their dimnames must have zero intersection 
#' @return an array
#' @author G.A.Paleologo  
#' @export 
join_all <- function(X){
  all <- TRUE
  X_dims <- vapply(X, function(x) length(dim(x)), 0L)
  n_dims <- X_dims[1]
  stopifnot(all_identical(X_dims))
  all.dim <- rep(all, n_dims)
  stopifnot (n_dims <= 5)
  stopifnot(all_identical(lapply(X, axes)))
  X_types <- vapply(X, function(a) class(a[1]), '')  
  stopifnot(all_identical(X_types))
  Y <- lapply(X, function(x){x[] <- 1L;x})
  Y_test <- accumulate(Y, all.dim = all.dim, FUN =`+`, na.value = 0)
  if (any(Y_test==2)) stop ('Overlapping elements when joining arrays.')
  if (X_types[1] %in% c('integer', 'numeric')) {
    out <- accumulate(X, all.dim = all.dim, FUN =`+`, na.value = 0)
  } else if (X_types[1] == 'boolean') {
    out <- accumulate(X, all.dim = all.dim, FUN =`|`, na.value = FALSE)
  }
  out[Y_test == 0] <- NA
  axes(out) <- axes(X[[1]])
  out    
}  

#' A simple array summary
#'
#' @param X array or matrix
#' @return side effect: print some data about the array
#' B
#' @export
summary.array <- function(X){
  X_first <- vapply(dimnames(X), head, '', n=1)
  X_last <- vapply(dimnames(X), tail, '', n=1)
  cat('type:\t', typeof(X), '\n')  
  cat(sprintf('%%NAs:\t%2.2f\n', sum(is.na(X))/length(X)*100))
  cat('-------------\n')
  if (is.null(axes(X))) stop('The axes for the array have not been set.')
  X_dims <- data.frame(axis=axes(X), 
                       dim=dim(X),
                       first=X_first,
                       `...`='...',
                       last=X_last)
  row.names(X_dims) <- NULL
  print(X_dims)
}

#' @export
summary.matrix <- summary.array