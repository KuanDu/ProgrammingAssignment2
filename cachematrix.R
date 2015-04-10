## These 2 functions are used to create a special object that stores
## a matrix and cache its inverse

## Below function "makeVector" is a wrapper of a matrix, which contais 4 fuctions to set/get
## the value of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The following function calculates the inverse of special matrix (wrapped in above function).
## If the inverse does not exist, calculate it and store it in cache; otherwise, get the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'n
  m <- x$getinverse()
  if(!is.null(m)){
    message("get cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
