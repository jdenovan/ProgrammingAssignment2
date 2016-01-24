## Repo: Coursera R Programming Assignment 2
## File: cachematrix.R
## Author: JDenovan
## Date: 23-Jan-2016

## Usage:
## > a <- matrix(rnorm(9), nrow=3, ncol=3, byrow=FALSE, dimnames=list(c("1","2","3"), c("a","b","c")) )
## > a2 <- makeCacheMatrix(a)
## > str(a2); names(a2)
## > cacheSolve(a2)
## calculating inverse...
##   
## > cacheSolve(a2)
## getting cached data...
##   

## Computing the inverse of a square matrix can be done with the solve function 
## in R. For example, if X is a square invertible matrix, then solve(X) returns its 
## inverse. This assignment optimizes the solve function by caching the result
## in an R environment using the <<- operator.

## Note that inupt matrix must be square and not singular

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data...")
        return(m)     ## last expression when inverse is cached
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    message("calculating inverse...")
    m            ## last expression when inverse is calculated
    
}
