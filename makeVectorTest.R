makeVectorTest <- function (x = numeric()) {
  m <- NULL
  
  ##define set function
  set <- function(y) {
    x <- y             ##does not work, new value is not set, use <<-
    m <- NULL          ##does not work, new value is not set, use <<-
  }
  
  ##define get function
  ##return the x value defined via the set value or constructor
  get <- function() x
  
  ##define set and get mean function
  setmean <- function(mean) m <-mean   ##does not work, new value is not set, use <<-
  getmean <-function() m
  
  list (set = set, get = get, setmean = setmean, getmean = getmean)
  
}

cachemeanTest <- function(x, ...) {
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