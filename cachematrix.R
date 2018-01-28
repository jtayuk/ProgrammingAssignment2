## Functions to take advantage of scoping rules in R to cache 
## time consuming operations, essentially by cacjing the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
      message("getting the cached result")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
## DEBUG/TESTING
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)
