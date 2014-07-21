## The function makeCacheMatrix creates a matrix object that is used to cache its inverse.
## 
## Function 'makeCacheMatrix' creates a matrix object
## 					Using 'set' allows the matrix values to be set
##					Using 'get' will retrieve the values from the matrix
## Function 'cacheSolve' takes the matrix created by the 'makeCacheMatrix' function and calculates its inverse
##					The setinv method caches the inverse calculated
##					The getinv method gets the inverse from the cache
##

## The function makeCacheMatrix creates a matrix object that is used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(matval) inv <<- matval
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
