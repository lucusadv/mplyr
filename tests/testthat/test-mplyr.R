library(Gmisc)
library(mplyr)

context("conversion")
test_that("From Data Frame to Array", {
  expect_identical(as.array(iris, sum, "Sepal.Length", "Species", "Petal.Width"), 
                   as.array(iris, sum,  Sepal.Length, Species, Petal.Width))
  expect_identical(as.array(iris, sum, "Sepal.Length", "Species", "Petal.Width")['virginica', '2.2'],
                   sum(iris[iris$Species=='virginica' & iris$Petal.Width==2.2, 'Sepal.Length']))
})


context("filtering")
addNames <- function(x){
  dimnames(x) <- list(ROWS=paste('R', 1:nrow(x), sep=''),
                      COLUMNS=paste('C', 1:ncol(x), sep=''))
  x[order(row.names(x)), order(colnames(x))]
}
test_that("subsetting by one dimension", {
  nr <- 14
  nc <- 8
  set.seed(3141592)
  A <- matrix(rnorm(nr*nc), nrow=nr, ncol=nc)
  A <- addNames(A)
  #  class(A) <- c('array','matrix')
  expect_identical(filter(A, ROWS %in% c('R1', 'R2')), 
                   A[row.names(A) %in% c('R1', 'R2'),])
  expect_identical(filter(A, ROWS %in% c('R1', 'R2')), 
                   A[row.names(A) %in% c('R1', 'R2'),])
  expect_identical(filter(A, COLUMNS %in% c('C1', 'C8')), 
                   A[,colnames(A) %in% c('C1', 'C8')])
  expect_identical(filter(A, COLUMNS %in% c('C1', 'C8')), 
                   A[,colnames(A) %in% c('C1', 'C8')])
  expect_identical(filter(A, COLUMNS =='C1'), 
                   A[,colnames(A) == 'C1', drop=FALSE])
  expect_identical(filter(A, COLUMNS=='C1'), 
                   A[,colnames(A) == 'C1', drop=FALSE])
  # on a table. Note that subsetting a table returns an array!
  # we set the class back to table. filter() preserves (correctly) class
  b1 <- structure(HairEyeColor[dimnames(HairEyeColor)[[1]] >= 'Brown',,], class='table')
  b2 <- filter(HairEyeColor, Hair >= 'Brown')
  expect_equal(b1, b2)
})

test_that("standard/nonstandard evaluation", {
  b1 <- filter(HairEyeColor, Hair >= 'Brown')
  b2 <- filter_(HairEyeColor, ~ Hair >= 'Brown')
  expect_equal(b1, b2)
})  



context("group by and aggregate")
test_that("subsetting by one dimension", {
  A <- array(1:7300, dim=c(365,10,2), dimnames=list(date=format(as.Date('1970-01-01')+1:365), 
                                                    stock=toupper(letters[1:10]),
                                                    feature=paste0('Feat',1:2)))  %>% 
    group_by(substr(date,1,7)) 
  A2 <- aggregate(A, FUN=mean, na.rm=TRUE)
  expect_equal(A2['1970-01', 'A', 'Feat1'], 
               mean(A[grep('^1970-01', dimnames(A)[[1]]), 'A', 'Feat1']))
})


test_that("aggregate", {
  data(asset_returns)
  ret_test1 <- asset_returns %>% 
    group_by(substr(date, 1, 7)) %>% 
    aggregate(FUN=sum) %>%
    filter(date == '2007-02', id == '000361105') %>%
    {.[,]}
  ret_test2 <- asset_returns[substr(row.names(asset_returns),1,7) == '2007-02', '000361105'] %>% sum
  expect_equal(ret_test1, ret_test2)
})

context("join and accumulate")
test_that("join", {
  nr <- 14
  nc <- 8
  set.seed(3141592)
  A <- matrix(rnorm(nr*nc), nrow=nr, ncol=nc)
  A <- addNames(A)
  
  X <- list(A[1:4, 1:4], A[5:14, 5:8])
  tmp1 <- join_all(X)
  tmp2 <- A
  tmp2[5:14, 1:4] <- NA
  tmp2[1:4, 5:8] <- NA
  expect_equal(tmp1, tmp2)
  expect_equal(axes(tmp1), axes(tmp2))
  expect_identical(dimnames(tmp1), dimnames(tmp2))
  
  X <- list(A[1:5, 1:4], A[5:14, 4:8])
  tmp <- tryCatch(join_all(X), error=function(cond) as.character(cond))
  # expect_error didn't work
  expect_equal(tmp, "Error in join_all(X): Overlapping elements when joining arrays.\n")
  
  X <- list(Titanic[1:2,,,], Titanic[3:4,1,,,drop=FALSE])
  Titanic2 <- join_all(X)
  Titanic2 <- Titanic2[dimnames(Titanic)[[1]], dimnames(Titanic)[[2]], 
                       dimnames(Titanic)[[3]], dimnames(Titanic)[[4]]]
  expect_equal(max(abs(Titanic2-Titanic), na.rm=TRUE), 0)
  
})

test_that("accumulate (sum) two matrices", {
  nr <- 200
  nc <- 200
  set.seed(3141592)
  A <- addNames(matrix(rnorm(nr*nc), nrow=nr, ncol=nc))[-1:-100, ]
  B <- addNames(matrix(rnorm(nr*nc), nrow=nr, ncol=nc))[, -1:-100]
  X <- list(A, B)
  tmp <- accumulate(X, all.dim = c(TRUE, TRUE), FUN =`+`, na.value = 0)
  common_rows <- intersect(row.names(A), row.names(B))
  common_cols <- intersect(colnames(A), colnames(B))
  tmp1 <- tmp[common_rows, common_cols]
  tmp2 <- A[common_rows, common_cols] + B[common_rows, common_cols]
  expect_equal(tmp1, tmp2)  
})

test_that("accumulate (sum) three arrays", { 
  X <- list(Titanic, Titanic, Titanic)
  tmp <- accumulate(X, all.dim = c(TRUE, TRUE, TRUE, TRUE), FUN =`+`, na.value = 0)
  tmp2 <- align_array(list(tmp, 3*Titanic), all.dim = c(TRUE, TRUE, TRUE, TRUE))
  expect_identical(tmp2[[1]], tmp2[[2]])
  
  X <- list(Titanic, Titanic, Titanic[-1,,,])
  tmp <- accumulate(X, all.dim = c(FALSE, TRUE, TRUE, TRUE), FUN =`+`, na.value = 0)
  tmp2 <- align_array(list(tmp, 3*Titanic[-1,,,]), all.dim = c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(tmp2[[1]], tmp2[[2]])  
})

test_that("accumulate (multiply) three arrays", {
  A1 <- array(1:36, dim=c(3, 3, 4), dimnames=list(A=letters[1:3], B=letters[4:6], C=letters[7:10]))
  A2 <- array(1:36, dim=c(3, 3, 4), dimnames=list(A=letters[2:4], B=letters[5:7], C=letters[7:10]))
  A3 <- array(1:36, dim=c(3, 3, 4), dimnames=list(A=letters[2:4], B=letters[5:7], C=letters[8:11]))
  X <- list(A1,A2,A3)
  dim1 <- Reduce(intersect, lapply(X, function(x) dimnames(x)[[1]]))
  dim2 <- Reduce(intersect, lapply(X, function(x) dimnames(x)[[2]]))
  dim3 <- Reduce(intersect, lapply(X, function(x) dimnames(x)[[3]]))
  A_expect <- A1[dim1, dim2, dim3] * A2[dim1, dim2, dim3] * A3[dim1, dim2, dim3]
  tmp <- accumulate(X, all.dim=c(FALSE, FALSE, FALSE), FUN =`*`, na.value = 1)
  tmp2 <- align_array(list(tmp, A_expect), all.dim = c(TRUE, TRUE, TRUE))
  expect_equal(tmp2[[1]], tmp2[[2]])
  
})

test_that("accumulate (add) two matrices, with missing data", {
  B1 <- B2 <- B3 <- addNames(matrix(0, nrow=4, ncol=3))
  
  B1 <- matrix(c(
    0,NA, 0,
    0, 2, 0,
    0, 4, 0,
    0, 0,NA
  ), nrow=4)
  
  B2 <- matrix(c(
    0, 0, 0,
    0, 3, 0,
    0, 0, 0,
    10,0, 0
  ), nrow=4)
  B1 <- addNames(B1)
  B2 <- addNames(B2)
  
  B3 <- B1 * B2
  X <- list(B1, B2)
  temp <- accumulate(X, all.dim=c(TRUE, TRUE), FUN =`*`, na.value = 1)
  expect_equal(temp, B3)
  
  B3 <- -2*B1
  X <- list(B1, B1)
  temp <- accumulate(X, all.dim=c(TRUE, TRUE), FUN =`-`, na.value = 0)
  expect_equal(temp, B3)
  
  B2a <- rbind(B2, 1:3)
  axes(B2a) <- axes(B1)
  temp1 <- accumulate(list(B1, B2), all.dim=c(FALSE, FALSE), FUN =`+`, na.value = 0)
  temp2 <- accumulate(list(B1, B2a), all.dim=c(FALSE, FALSE), FUN =`+`, na.value = 0)
  expect_equal(temp1, temp2)
})

context("align and overlay")
test_that("align by one dimension", {
  data(asset_returns)
  A <- asset_returns[1:5,1:5]
  B <- asset_returns[11:15,1:5]
  tmp <- align_array(list(A, B), all.dim=c(FALSE, FALSE))
  expect_equal(tmp[[1]],  tmp[[2]])
  tmp <- align_array(list(A, B), all.dim=c(TRUE, FALSE))
  expect_equal(tmp[[1]]['2007-01-03', '000360206'],  asset_returns['2007-01-03', '000360206'])
  expect_equal(tmp[[2]]['2007-01-03', '000360206'],  NA_real_)
  expect_equal(overlay(tmp[[1]], tmp[[2]]), asset_returns[c(1:5,11:15),1:5])
  expect_equal(overlay(asset_returns[1:10,1:10], asset_returns[6:20,1:10]), asset_returns[1:20,1:10])  
})
# 
# context("utility functions")
# test_that("vector-matrix-dataframe-list conversion", {
#   data(asset_returns)
#   A <- m2df(asset_returns, key='date')
#   expect_equal(row.names(asset_returns),  A$date)
#   expect_equivalent(A$`000361105`,  asset_returns[,'000361105'])
#   expect_equal(m2v(asset_returns[,'000361105', drop=FALSE]),  asset_returns[,'000361105'])
#   expect_equal(m2l(asset_returns)[['2007-05-25']],  asset_returns['2007-05-25',])
#   expect_equal(m2l(asset_returns, by.row=FALSE)[['000361105']],  asset_returns[,])
#   expect_equal(df2c(iris, mean, "Sepal.Length", "Species", "Petal.Width")['setosa', '0.1'], 
#                iris %>% filter(Petal.Width==0.1, Species=='setosa') %>% select(Sepal.Length) %>% {mean(.[,])})
# })
# 
