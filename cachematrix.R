## This function intends to cache the inverse of a matrix, because matrix inverstion is usually a costly computation.  
## Caching the inverse of a matrix will consume less time than computing the matrix repeatedly.
## This "makeCacheMatrix" function is used to create a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This "cacheSolve" function is used to calcuate the inverse of the special matrix created by the "makeCacheMatrix" function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}