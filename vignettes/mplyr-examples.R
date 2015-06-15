## ------------------------------------------------------------------------
library(mplyr)
A <- as.array(diamonds, mean, x, clarity, cut)
B  <- as.array(diamonds, mean, x, clarity, cut, color)
A

## ------------------------------------------------------------------------
summary(A)
summary(B)

## ------------------------------------------------------------------------
library(mplyr)
A <- as.array(diamonds, sum, x, clarity, cut)
axes(A)
axes(A) <- toupper(axes(A))
A %<>% set_axes(tolower(axes(A)))

## ------------------------------------------------------------------------
A %>% filter(clarity %in% c('I1', 'SI2'), cut %in% c('Good','Very Good'))

## ------------------------------------------------------------------------
A %>% 
  filter_(~clarity %in% c('I1', 'SI2')) %>% 
  filter_("cut %in% c('Good','Very Good')")

## ------------------------------------------------------------------------
A <- array(1:7300, dim=c(365,10,2), dimnames=list(date=format(as.Date('1970-01-01')+1:365), 
                                                  stock=toupper(letters[1:10]),
                                                  feature=paste0('Feat',1:2)))  
summary(A)
A %<>% 
  group_by(substr(date,1,7)) %>%
  aggregate(FUN=mean, na.rm=TRUE)
summary(A)

## ------------------------------------------------------------------------
  # we use vadr for unpacking
  library(vadr)
  X0 <- structure(Titanic, class='array')
  X1 <- X0[1:2,,,]
  X2 <- X0[-(1:2),,,]
  X3 <- accumulate(list(X1,X2), all.dim = c(TRUE, TRUE, TRUE, TRUE), FUN =`+`, na.value = 0)
  # let's align axes first
  bind[X3, X0] <- align_array(list(X3, X0), all.dim=c(T,T,T,T))
  table(X0 == X3)

## ------------------------------------------------------------------------
  library(vadr)
  X0 <- structure(Titanic, class='array')
  X1 <- X0[1:2,,1,1]
  X2 <- X0[-(1:2),,1,1]

  accumulate(list(X1,X2), all.dim = c(TRUE, TRUE), FUN =`+`, na.value = 0)
  join_all(list(X1, X2))

