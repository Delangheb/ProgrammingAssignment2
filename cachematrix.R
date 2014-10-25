makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ##define set function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##define get function
  ##return the x value defined via the set value or constructor
  get <- function() x
  
  ##define set and get mean function
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function() inv
  
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("geting cached data")
    return(inverse)
  }
  
  ##data not in cache
  data <- x$get()
  inverse <- solve(data, ...)   ##calculate inverse matrix
  x$setinverse(inverse)         ##save inverse in cache   
  inverse                       ##return the inverse
  
}
