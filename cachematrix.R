##Assignment: Caching the Inverse of a Matrix
##Matrix inversion is usually a costly computation and there may be some benefit to caching 
##the inverse of a matrix rather than computing it repeatedly (there are also alternatives
##to matrix inversion that we will not discuss here). Your assignment is to write a pair of 
##functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) m <<- solve
  get_inv <- function() m
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$get_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv(m)
  m
}
