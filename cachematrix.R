## The following script executes the function that caches the inverse of matrix rather than computing it repeatedly to avoid matrix inversion.

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  hc <- NULL
  setMatrix <- function(y) {
    x <<- y
    hc <<- NULL
  }
  getMatrix <- function() x
  setinverse <- function(inv) hc <<- inv
  getinverse <- function() hc
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  hc <- x$getinverse()
  if (!is.null(hc)) {
    message("getting cached inverse matrix")
    return(hc)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
