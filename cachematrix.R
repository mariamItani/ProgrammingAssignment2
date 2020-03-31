## Below are two functions that together cache the inverse of a matrix
## assuming that the matrix supplied is always invertible

##Function 1: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  M_inv <- NULL
  set <- function(y) {
    x <<- y
    M_inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) M_inv <<- solve
  getinv <- function() M_inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv )
}


## Function 2: cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        M_inv <- x$getinv()
    if(!is.null(M_inv)) {
      message("getting cached data")
      return(M_inv)
    }
    data <- x$get()
    M_inv <- solve(data, ...)
    x$setinv(M_inv)
    M_inv
  }
  
