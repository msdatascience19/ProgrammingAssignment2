## computes the inverse values of a matrix and cache the result
##
## example usage:
## source("cachematrix.R")
## matrix <- makeCacheMatrix(matrix(1:4,2,2))
## matrix$get() ## shows the matrix values
## cacheSolve(matrix)
##
## in the first run of cacheSolve(matrix) the inverse matrix result is computed.
## running the function again shows the cached value
##
## you can use matrix$set(matrix(1:100, 100, 100)) to set new matrix data and clear the cache

## defines functions for storing matrix information and stores matrix data if given in parameter
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize cache variable for inversed matrix
    inversedMatrix <- NULL
    
    ## define set function for storing a given matrix
    set <- function(y) {
        x <<- y
        inversedMatrix <<- NULL
    }
    
    ## define get function for showing the value of the stored matrix
    get <- function() x
    
    ## define setInverse function for storing a given inversed matrix value
    setInverse <- function(setValue) inversedMatrix <<- setValue
    
    ## define getInverse function for showing the value of the stored inversed matrix value
    getInverse <- function() inversedMatrix
    
    ## return a list of defined functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
## use cached value if exist or calculate inverse matrix and cache calculated data
cacheSolve <- function(x, ...) {
        
    ## try to get cached data
    inversedMatrix <- x$getInverse()
    if(!is.null(inversedMatrix)) {
        ## cached data found -> return cached data
        message("getting cached data")
        return(inversedMatrix)
    }
    
    ## no cached data
    ## get matrix
    data <- x$get()
    ## compute inversed matrix with solve function
    inversedMatrix <- solve(data, ...)
    ## store result in cache
    x$setInverse(inversedMatrix)
    ## return result
    inversedMatrix 
}