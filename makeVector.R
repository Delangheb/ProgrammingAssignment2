makeVector <- function (x = numeric()) {
  m <- NULL
  
  ##define set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##define get function
  ##return the x value defined via the set value or constructor
  get <- function() x
  
  ##define set and get mean function
  setmean <- function(mean) m <<-mean
  getmean <-function() m
  
  list (set = set, get = get, setmean = setmean, getmean = getmean)
  
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("geting cached data")
    return(m)
  }
  
  ##data not in cache
  data <- x$get()
  m <- mean(data, ...) ##calculate mean
  x$setmean(m)         ##save mean in cache   
  m                    ##return the mean
}