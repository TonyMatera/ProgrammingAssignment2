## The following functions are intended to calculate the
## inverse of a matrix and cache it in a separate
## environment. It is then able to pull the stored value
## the following times the inverse needs to be calculated.
## Being able to pull the cached data saves time as the
## inverse will not be needed to be calculated again.


## This function, "makeCacheMatrix", creates a list
## containing a function to set the value of the matrix,
## get the value of the matrix, set the value of the
## inverse matrix, and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y = matrix()) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function, "cacheSolve", calculates the inverse
## of the matrix set above by first checking to see if
## the inverse has already been calculated (i.e. cached).
## If it has, it pulls the stored value. If it has not,
## it calculates it for the first time and sets it in
## the cache using the "setinv" function.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("Standby: retrieving cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}


## Note: These functions assume the original matrix is
## both square and invertible.