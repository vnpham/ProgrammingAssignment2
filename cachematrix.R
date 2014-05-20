## This project demonstrate the capability to use R's lexical scoping as
## a mean to cache computed value.  Specifically, this project provide 2
## functions to create and get the cache value of the inverse of a matrix
###########################################################################

## Create a list that contain a matrix with a place holder for the
## cache value of its inverse
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL # inverse matrix initialized to NULL
   
   # access functions
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv
   
   # return value
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Compute and cache or simply get the cache value of the inverse
## matrix
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
   inv <- x$getinv()   # get the currently cached inverse matrix
   
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   
   # if inverse matrix hasn't been computed
   # compute inverse matrix and store to cache
   data <- x$get()
   inv <- solve(data, ...)
   x$setinv(inv)
   
   inv  # return
}

# This is an extra function used to test the matrix inverse caching mechanism
# - test message "getting cached data" printed to make sure cache mechanism is used
# - print out the multiplication of the matrix and its inverse to visually see that
#      it is close to the identity matrix
testCacheInv <- function () {
   cm <- makeCacheMatrix(matrix(runif(9),nrow=3)) # create a random 3x3 matrix
   
   inv1 <- cacheSolve(cm)      # calculate and cache the inverse
   
   # get the inverse from cache
   # message "getting cached data" should be printed
   inv2 <- cacheSolve(cm)
   
   message("\noutput validation: result should be approximately the identity matrix")
   cm$get() %*% inv2
}
