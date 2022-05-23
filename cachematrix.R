## Put comments here that give an overall description of what your
## functions do

## This function follows the same structure as the sample solution and creates a list that contains a function to:
## set the value of the vector
## get the value of the vector
## set the value of the matrix
## get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix created by Function #1, but first checks to see if (and retrieves) 
## the matrix has already been calculated in order to avoid a computation step.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


