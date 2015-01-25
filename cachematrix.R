##This functions avoid the need for recalculate an already 
##calculated inverse from a matrix, getting the result 
##from the cache

## Creates a new type of matrix containing 4 functions for:
## setting the matrix
## getting the matrix
## setting the inverse
## getting the inverse
## It's a lot like the vectormean example

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function checks if we already have the inverse of the matrix, if we already have
# it,it get it from the cache, otherwise the inverse is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
}
