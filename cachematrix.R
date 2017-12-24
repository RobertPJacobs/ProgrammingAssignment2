## makeCacheMatrix
## creates a cached matrix
## solves the matrix by calculating the inverse but first checks if inverse has not been calculated already


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## initiliase the s variable
        s <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## get the value of the matrix 
        get <- function() x
        ## calc the inverse of the matrix and cache the result
        setinverse <- function(solve) s <<- solve 
        ## get the value of the solve
        getinverse <- function() s
        ## create the list
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        ## check to see if data is cached
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
                ## quit the function at this point
        }
        ## continue if function did not end
        data <- x$get()
        s <- solve(data, ...)
        ## save (set) the cached data for next use
        x$setinverse(s)
        ## return the inversed matrix
        s
}

## Test functions use the following:

## source("cacheMatrix.R")
## m <- makeCacheMatrix(matrix(rnorm(15), nrow = 3, ncol = 3))
## m$get()
## m$getinverse()
## cacheSolve(m)
## m$getinverse()  # only to show you that the inverste has been stored and does not affect anything
## cacheSolve(m)


