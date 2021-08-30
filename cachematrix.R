## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
  s <- function(y) {
          x <<- y
          i <<- NULL

}
g <- function() x
  sinverse <- function(inverse) i <<- inverse
  ginverse <- function() i
  list(s = s,
       g = g,
       sinverse = sinverse,
       ginverse = ginverse)
}

## The inverse of the special "matrix" returned by makeCacheMatrix is computed with this function. If the inverse has been calculated before (and the matrix hasn't changed), cacheSolve should get it from the cache.

cacheSolve <- function(x, ...) {
       i <- x$genverse()
  if (!is.null(i)) {
          message("Loading the cached data")
          return(i)
  }
  data <- x$g()
  i <- solve(data, ...)
  x$sinverse(i)
  i
}
}
D <- matrix(c(20,42,73,54),2,2)
D0 <- makeCacheMatrix(D)
cacheSolve(D0)
