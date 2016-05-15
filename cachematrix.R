## The following function creates a special matrix object whose inverse can be stored in the cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## inverse stored as null
    set <- function(y) { ## Set matrix value and reassign NULL to inverse
        x <<- y
        m <<- NULL
    }
    get <- function() x ## Use get function to retrieve matrix value
    setinv <- function(solve) m <<- solve ## Use solve function to set the inverse of matrix
    getinv <- function() m ## Use get function to retrive inverse of matrix value
    list(set = set, get = get, ## return list of defined functions
         setinv = setinv, 
         getinv = getinv)
}


## The following function checks to see if the inverse of a matrix has already been calculated and stored in the cache, if not it generates the inverse of the matrix and returns that.
cacheSolve <- function(x, ...) {
    m <- x$getinv() ## check if a cached inverse of the matrix exists  
    if(!is.null(m)) {
        message("getting cached data") ## if yes, show message and return inverse from cache
        return(m)
    }
    data <- x$get() ## if no data in cache, solve for inverse, and return result.
    m <- solve(data, ...)
    x$setinv(m)
    m
}


## For this exercise, I used the following matrix for testing purposes
## My_Matrix <- matrix(c(5, 6, 7, 1, 6, 3, 8, 2, 9), nrow=3, ncol=3)
## My_CacheMatrix <-  makeCacheMatrix(My_Matrix)
## cacheSolve(My_CacheMatrix)
## cacheSolve(My_CacheMatrix)