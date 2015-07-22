#' a simple decorator to benchmark execution times
#' 
#' @param f function
#' @return a function. Identical to f, but prints execution time 
#' 
timeit <- function(f){
  a <- deparse(substitute(f))
  function(...) {
    cat("calling function:", a, '\n')
    a <- system.time({x <- f(...)})[['user.self']]
    cat('time elapsed:', a, '\n')
    x
  }
}

#' creates names for a vector equal to its elements
#'
#' @param x vector
#' @return a vector with names equal to its elements 
#' @author G.A.Paleologo  
#' 
withNames <- function(x){
  names(x) <- x
  x
}

# `setrownames<-` <- function (x, ...) {
#   UseMethod("rownames<-", x)
# }
# `setcolnames<-` <- function (x, ...) {
#   UseMethod("colnames<-", x)
# }


#' given a vector v and an elements x, returns
#' the next element after x in v
#'
#' @param v vector. All elements must be different.
#' @param x element
#' @return an element of v 
#' @author G.A.Paleologo  
#' 
succ <- function(x, v){
  stopifnot(sum(duplicated(v)) == 0)
  stopifnot(x %in% v)
  if (x == tail(v, 1)){
    return(NA)
  } else {
    return(v[min(which(v > x))])
  }
}

#' given a vector v and an elements x, returns
#' the previous element before x in v
#'
#' @param v vector. All elements must be different.
#' @param x element
#' @return an element of v 
#' @author G.A.Paleologo  
#' 
prev <- function(x, v){
  stopifnot(sum(duplicated(v)) == 0)
  stopifnot(x %in% v)
  if (x == head(v, 1)){
    return(NA)
  } else {
    return(v[max(which(v < x))])
  }
}



#' applies intersect to multiple arguments
#' 
#' @param ... multiple arguments to which the intersect operation can be applied
#' @return a vector/set equal to the intersection of the input sets
#'
#' @author G.A.Paleologo
#' 
mintersect <- function(...){
  Reduce(intersect, list(...))
}

#' symmetric set difference
#' 
#' @param x set
#' @param y set
#' @return a vector/set equal to the symmetric set difference of the input sets
#'
#' @author G.A.Paleologo
#' 
symmsetdiff <- function(x, y){
  union(setdiff(x, y), setdiff(y, x))
}


#' applies union to multiple arguments
#' 
#' @param ... multiple arguments to which the intersect operation can be applied
#' @return a vector/set equal to the union of the input sets
#'
#' @author G.A.Paleologo 
#' 
munion <- function(...){
  Reduce(union, list(...))
}

#' computes symmetric set difference
#' 
#' @param ... multiple arguments to which the intersect operation can be applied
#' @return a vector/set equal to the symmetricsetdifference of the input sets
#'
#' @author G.A.Paleologo 
#' 
symmsetdiff <- function(x, y){
  union(setdiff(x, y), setdiff(y, x))
}


#' the "not in" operator
#' 
#' @param x vector
#' @param y vector
#' @return you guessed it: a vector
#' 
`%nin%` <- function(x,y) !(x %in% y)

#' converts a matrix into a data frame  
#' 
#' converts a matrix into a data frame by adding a first
#' column equal to the row names
#'
#' @param x matrix
#' @param key string, optional name of the first column
#' @return a data frame 
#'
#' @author G.A.Paleologo  
#' 
m2df <- function(x, key = NA){
  stopifnot(is.matrix(x))  
  out_colnames <- c(key, colnames(x))
  out <- data.frame(row.names(x), x, stringsAsFactors=FALSE)
  names(out) <- out_colnames  
  return(out)
}


#' converts a data frame into a matrix  
#' 
#' converts a data frame into a matrix by using a column
#' (by default, the first) to set the matrix row names.
#' The column is removed from the original data frame
#'
#' @param x data frame
#' @param key character, optional name of the column to be used
#' @return a matrix 
#'
#' @author G.A.Paleologo  
#' 
df2m <- function(x, key = NULL){
  stopifnot(is.data.frame(x)) 
  if (is.null(key)) {
    ind <- 1
  } else {
    ind <- which(names(x) == key)
    stopifnot(length(ind) == 1)
  }
  
  out_rownames <- as.data.frame(x)[,ind]
  out <- as.matrix(x[, -ind])
  row.names(out) <- out_rownames
  return(out)
}


#' converts a data table into a matrix  
#' 
#' converts a data table into a matrix by using a column
#' (by default, the first) to set the matrix row names.
#' The column is removed from the original data frame
#'
#' @param x data table
#' @param key character, optional name of the column to be used
#' @return a matrix 
#'
#' @author G.A.Paleologo  
#' 
dt2m <- function(x, key = NULL){
  stopifnot(is.data.table(x)) 
  x <- as.data.frame(x)
  df2m(x,key)
}

#' converts an n x 1 matrix into a vector
#'
#' @param x matrix
#' @return numerical vector
#'
#' @author G.A.Paleologo  
#' 
m2v <- function(x){
  stopifnot(is.matrix(x) & ncol(x) == 1)
  structure(x[,], names=row.names(x))
}

#' converts an n x 1 data frame into a vector  
#'
#' @param x data.frame
#' @return numerical vectors
#'
#' @author G.A.Paleologo  
#' 
df2v <- function(x){
  stopifnot(is.data.frame(x) & ncol(x) == 1)
  m2v(as.matrix(x))
}

#' converts a vector into an n x 1 data frame   
#'
#' @param x vector
#' @param key character, optional name of first column
#' @return data.frame
#'
#' @author G.A.Paleologo  
#' 
v2df <- function(x, keys=c('V1', 'V2')){
  x <- data.frame(names(x), x, stringsAsFactors=FALSE)
  names(x) <- keys
  x
}

#' converts a matrix into a ragged array object 
#' 
#' converts a matrix into a ragged array object, i.e., a list of vectors; 
#' each vector is a row or column of the original matrix
#' 
#' @param x matrix
#' @param by.row boolean, should the vectors be rows
#' @return a ragged array
#'
#' @author G.A.Paleologo 
#' 
m2l <- function(x, by.row = TRUE){
  stopifnot(is.matrix(x))  
  if (by.row) x <- t(x)
  out <- as.list(data.frame(x))
  out <- lapply(out, function(y){names(y) <- row.names(x); y})
  names(out) <- colnames(x)
  class(out) <- c('raggedarray', 'list')
  return(out)
}

#' converts a matrix into a zoo object
#' 
#' @param x matrix
#' @param by.row boolean, are the dates rows
#' @return a zoo object
#'
#' @author G.A.Paleologo 
#' 
m2zoo <- function(x, by.row = TRUE){
  stopifnot('matrix' %in% class(x))  
  if (!by.row) x <- t(x)
  out <- zoo(x, order.by=as.Date(row.names(x)))
  return(out)
}

#' converts a raggedarray or list of vectors into a matrix
#' 
#' @param x raggedarray or list of vectors
#' @param by.row boolean, should the vectors be rows
#' @return a matrix
#'
#' @author G.A.Paleologo 
#' 
l2m <- function(x, by.row = TRUE){  
  stopifnot('list' %in% class(x))  
  x_names <- lapply(x, names)
  if (any(sapply(x_names, function(x) any(duplicated(x)))))
    stop('duplicate names in the vectors: unreliable output')
  row_names <- sort(Reduce(union, x_names))
  col_names <- sort(names(x)) 
  out <- matrix(NA, nrow = length(row_names), ncol = length(col_names), 
                dimnames = list(row_names, col_names))
  for (i in names(x)){
    out[names(x[[i]]), i] <- x[[i]]
  }
  if (by.row) out <- t(out)
  return(out)
}

#' Premultiplies a diagonal matrix by a matrix.
#'
#' @param x numeric, representing the diagonal of the matrix
#' @param Y matrix, having a number of rows equal to the length of x
#' @return a matrix, equal to \code{diag(x) \%*\% Y} 
#'
#' @author G.A.Paleologo 
#' 
`%D*%` <- function(x, Y) {
  n <- nrow(Y)  
  for (i in 1:n){
    Y[i, ] <- Y[i, ] * x[i] 
  } 
  return(Y)
}


#' Postmultiplies a diagonal matrix by a matrix.
#'
#' @param x numeric, representing the diagonal of the matrix
#' @param Y matrix, having a number of rows equal to the length of x
#' @return a matrix, equal to \code{diag(x) \%*\% Y} 
#'
#' @author G.A.Paleologo 
#' 
`%*D%` <- function(Y, x) {
  n <- ncol(Y)  
  for (i in 1:n){
    Y[, i] <- Y[, i] * x[i] 
  } 
  return(Y)
}


#' applies a function to two vectors element-wise, aligning them
#'
#' @param x numeric
#' @param y numeric
#' @param all boolean. Should this be applied to all elements?
#' @param all.x boolean. Should all the elements of the first vector be used?
#' @param all.y boolean. Should all the elements of the second vector be used?
#' @param na.values numeric. Which value should be used in place of NAs?
#' @return a vector 
#' @note \code{opVectors} requires that vectors have names for their elements.
#'
#' @author G.A.Paleologo  
#' 
#' @examples 
#' a <- b <- seq(letters)
#' names(a) <- names(b) <- letters
#' opVectors(a[6:15], a[1:10], all.x = TRUE)
#' opVectors(a[6:15], a[1:10], FUN = `/`)
opVectors <- function(x, y, all = FALSE, all.x = all, 
                      all.y = all, na.values = 0, FUN = `+`, ...){
  x_names <- names(x)
  y_names <- names(y)
  if (all.x & all.y) {
    z_names <- union(x_names, y_names)
  } else if (all.x  & !all.y){
    z_names <- x_names
  } else if (!all.x & all.y){
    z_names <- y_names
  } else if (!all.x & !all.y){
    z_names <- intersect(x_names, y_names)
  }
  x2 <- y2 <- rep(na.values, length(z_names))
  names(x2) <- names(y2) <- z_names
  temp_names <- intersect(z_names, x_names)
  x2[temp_names] <- x[temp_names]
  temp_names <- intersect(z_names, y_names)
  y2[temp_names] <- y[temp_names]
  FUN(x2, y2, ...)
}
 
#' Converts a tree represented as a list as a data frame. 
#' 
#' @param x list
#' @param colnames character vector column names of the first column
#' @return a data frame 
#' @author G.A.Paleologo  
#' 
flattenList <- function(x, colnames=NULL){
  x <- unlist(x)
  x_names <- sapply(names(x), strsplit, split='\\.') 
  n_cols <- max(sapply(x_names, length))
  x_names <- lapply(x_names, function(x) c(x, rep(NA, n_cols-length(x))))
  x_df <- as.data.frame(do.call(rbind, x_names))
  x_df$value <- x
  if (!is.null(colnames)) names(x_df)[1:length(colnames)] <- colnames
  return(x_df)
}

#' @title subsets a ragged array
#' 
#' @param x a ragged array
#' @param v a ragged array containing elements named
#'  with names in x, and values of each vector in v 
#'  in the names of a corresponding element of x
#' @return a ragged array 
#' @author G.A.Paleologo  
#' 
subset.raggedarray <- function(x, v){
  tstamp_common <- intersect(names(x), names(v))
  x_class <- class(x)
  x <- x[tstamp_common]
  for (d in tstamp_common){
    ind <- intersect(v[[d]], names(x[[d]]))
    x[[d]] <- x[[d]][ind]
  }
  class(x) <- x_class
  x
}

#' summarizes a ragged array
#' 
#' @param x ragged array
#' @return a matrix with the summary data 
#' @author G.A.Paleologo 
#' 
summary.raggedarray <- function(x){
  out <- list()
  out[['length']] <- summary(sapply(x, length))
  type_flag <- all(sapply(x, class) %in% c('numeric', 'integer', 'logical'))
  if (type_flag) {
    out[['mean']] <- summary(sapply(rets, mean, na.rm=TRUE))
    #out[['max']]   <- summary(sapply(rets, max, na.rm=TRUE))
    #out[['min']]   <- summary(sapply(rets, min, na.rm=TRUE))
    out[['mad']]   <- summary(sapply(rets, mad, na.rm=TRUE))
  }
  out[['NA.obs']] <- summary(sapply(x, function(y) sum(is.na(y))))
  round(do.call(rbind, out), 3)
}


#' checks that all items in a list are identical
#' 
#' @param x list
#' @return a boolean 
#' @author G.A.Paleologo 
#' 
all_identical <- function(X){
  y <- TRUE
  for (i in 2:length(X)){
    y <- y & identical(X[[i-1]], X[[i]])
  }
  y
}

#' checks that all items in a list are equal
#' 
#' @param x list
#' @return a boolean 
#' @author G.A.Paleologo 
#' 
all_equal <- function(X){
  y <- TRUE
  for (i in 2:length(X)){
    y <- y & all.equal(X[[i-1]], X[[i]])
  }
  y
}

#' Fast last-observation carried forward for matrices
#' 
#' @param x matrix
#' @param memory integer, how long missing data should be carried forward?
#' @return a matrix
#' 
nalocf <- function(x, memory=72L){
  n <- nrow(x)
  missing_counter <- rep(0, ncol(x))
  for (i in 2L:n){
    mask <- is.na(x[i,])
    missing_counter[mask] <- missing_counter[mask] + 1
    missing_counter[!mask] <- 0
    x[i, mask] <- x[i-1, mask]
    x[i, missing_counter > memory] <- NA
  }
  x
}

#' cuts a character vector using a set of character values
#' 
#' @param x character vector
#' @param v character vector
#' @param align character, either 'right' or 'left'
#' @return a character vector
#' @examples
#'  v <- letters[seq(2,length(letters),2)]
#'  cut(letters[1:10], v, 'right')
#'  cut(letters[1:10], v, 'left')
#'   
cut_character <- function(x, v, align=c('left', 'right')){
  align <- match.arg(align)
  if (align == 'left'){
    out <- sapply(x, function(i) max(v[v <= i])   ) 
  } else {
    out <- sapply(x, function(i) min(v[v >= i])   ) 
  } 
  out  
}

#' @title Reverse the order of a vector
#' 
#' @param X vector
#' @return vector
#' 
#' @examples
#' v <- letters[1:10]
#' Reverse(v)
#' 
#' 

Reverse <- function(X) X[length(X):1]

#' Given as an put a list of vectors and an integer, averages the n
#' trailing adjacent list elements in a list of vectors;
#' the first n elements are only averaged partially.
#
#' @param x list of vectors. elements must be named and vector elements
#' must be named
#' @param n number of trailing elements
#' @return a list of vectors
#' 
avg_list <- function(x, n){
  stopifnot(n < length(x))
  Y <- list()
  X <- l2m(x, by.row=TRUE)
  X_colnames <- colnames(X)
  X[is.na(X)] <- 0
  Y[[1]] <- X
  for (i in 2:n){
    X <- cbind(X[,1], X[, -ncol(X)])
    colnames(X) <- X_colnames
    Y[[i]] <- X
  }
  out <- (Reduce(`+`, Y)/n) %>% 
    m2l %>%
    lapply(na.omit)
  out
}

#' utility function to create lists with repeated elements
#' creates a list with length(x) elements, all identically equal to v
#' the names of the list elements are the names of x
#'
#'@param x character vector
#'@param v object
#'@return a list
#'
#'
repl <- function(x, v){
  lapply(withNames(x), function(a) v)
}


#' multivariate plotting
#' 
#' @param X matrix, 
#' @return a plot
#' @author G.A.Paleologo
#' 
MVTSPlot <- function(X, obs = c('rows','cols'), lowcol = 'red', highcol ='green'){
  # X is a matrix
  # X <- matrix(rnorm(300), ncol=50)
  # colnames(X) <- as.character(as.Date('2010-01-01') + seq(ncol(X)))
  # row.names(X) <- paste('V', seq(nrow(X)), sep='')
  # MVTSPlot(X)
  obs <- match.arg(obs)
  if (obs == 'rows') X <- t(X)
  #
  X2 <- melt(X)
  names(X2) <- c('subjVar', 'obsVar', 'value')
  X2$obsVar <- as.Date(X2$obsVar)  
  X2$subjVar <- as.factor(X2$subjVar)
  # median per observation
  f <- function(x){
    x$medianValue <-  median(x$value, na.rm=TRUE)
    x
  }  
  X2 <- ddply(X2, .(subjVar), f)
  base_size <- 10
  temp <- unique(X2$obsVar)
  dateInterval <- round(ncol(X)/5, 0)
  X2.breaks <- temp[seq(from=1, to=length(temp)-round(dateInterval/2,0), by=dateInterval)]
  p1 <- ggplot(X2, aes(x=factor(obsVar), y=subjVar, fill=value)) + 
    geom_tile() + 
    scale_fill_gradient(low=lowcol, high=highcol) +
    scale_x_discrete(expand = c(0, 0), breaks=factor(X2.breaks)) +
    scale_y_discrete(expand = c(0, 0))  +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none", 
          axis.ticks  =element_blank(), 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = base_size , angle = 0, hjust = 0, colour = "grey50"))
  X3 <- as.data.frame(apply(X, 2, mean, na.rm=TRUE))
  names(X3) <- "y"
  X3$x <- as.Date(as.character(row.names(X3)))
  #
  p2 <- ggplot(X2, aes(y = value, x = subjVar)) +
    scale_fill_gradient(low="green", high="red") +
    geom_boxplot(aes(fill=(medianValue))) + 
    scale_x_discrete(expand = c(0, 0)) +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = base_size , vjust = 0, hjust = 0, colour = "grey50")) +
    coord_flip() 
  #scale_y_continuous(expand = c(0, 0)) 
  p3 <- ggplot(X3, aes(y = y, x = as.numeric(x))) +
    theme(axis.ticks = element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) +
    scale_x_continuous(expand = c(0, 0)) +
    geom_smooth(colour = "darkblue", method = "loess") 
  #        
  p4 <- ggplot(X3, aes(y = y, x = "a")) +
    geom_boxplot(color = "darkblue") + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_blank(), 
          legend.position = "none",
          axis.ticks = element_blank()
    )
  grid.arrange(p1, p2, p3, p4, heights=c(2/3, 1/3), widths=c(2/3,  1/3),ncol=2, nrow=2)
}


#' sequential list processing. 
#' 
#' 
#' 
#' Given a list X and a function FUN(x1,x2)
#' computes Z[[i]] <- FUN(X[[i-1]], X[[i-1]])
#' 
#' 
#' @param X1 list
#' @param FUN a function
#' @param ... additional elements to give to FUN
#' @return a list
#' @author G.A.Paleologo
#' @examples
#' X <- list(a=1,b=2, c=NA, d=4,e=5)
#' FUN <- function(x,y, ...) mean(c(x,y), ...)
#' procList(X, FUN, na.rm=F)
#' 
#' 
lapplyseq <- function(X, FUN, label=c('first','last'),...){
  label <- match.arg(label)
  stopifnot('list' %in% class(X))
  Y <- lapply( 2:length(X), function(i) FUN(X[[i-1]], X[[i]], ...) )
  names(Y) <- ifelse(label =='last', names(X)[-1], names(X)[-length(X)])
  Y
}

#' This function prints summary statistics of a ragged array
#' i.e. a list f vectors of unequal length. Mostly used with
#' lists of portfolios

#' @param X list, a list comprised of vectors coercible to numeric
#' @return the function has only side effects
#' 
summary_raggedarray <- function(X){
  X <- l2m(X)
  summary_data <- data.frame(n_elements = nrow(X),
                             mean_sum = mean(apply(X, 1, sum, na.rm=TRUE)),
                             mean_L1 = mean(apply(X, 1, l(x ~ sum(abs(x), na.rm=TRUE)))),
                             mean_L2 = mean(apply(X, 1, l(x ~ sum(x^2, na.rm=TRUE))))) %>%
    as.matrix %>% t %>% (l(x ~ {colnames(x) <- 'value'; x}))
  cat("\n---summary data----\n")
  print(summary_data)
}
