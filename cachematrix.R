## Put comments here that give an overall description of what your
## functions do

## Since Matrix inversion is expensive to  compute we write 2 functions for  
## caching the inverse of a matrix so that the inverse need not be computed again

## Write a short comment describing this function

## function creates a special "matrix" object that can cache its inverse. Which 
## is a list containing functions to set, get, setting the inverse and getting the 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInv <- function(inverse) {
                inv <<- inverse
        }
        getInv <- function() {inv}
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been computed before 
# then the it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv() 
        if(!is.null(inv)) {
                message("Obtaining from cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(x, ...)
        x$setInv(inv)
        inv
}
