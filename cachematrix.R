## The basic purpose of the functions defined below is to compute and cache
## the inverse of a given matrix. 

## This function creates a special "matrix" object that can cache its inverse.
## If the inverse matrix is already calculated, it will be returned from the cache.

makeCacheMatrix <- function(x = matrix()) {
        inverseX <- NULL
        set <- function(y) {
          x <<- y
          inverseX <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inverseX <<-inverse
        getinverse <- function() inverseX
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix returned by 'makeCacheMatrix' function.
## If the inverse is already available, it will be retreived from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseX <- x$getinverse()
        if (!is.null(inverseX)) {
          message("Retrieving cached inverse matrix")
          return(inverseX)
        } else {
          inverseX <- solve(x$get())
          x$setinverse(inverseX)
          return(inverseX)
        }
}
