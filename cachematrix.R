## Put comments here that give an overall description of what your
## functions do

## This function follows the same structure as the sample solution and creates a list that contains a function to:
## get and set the value of a matrix, and then get and set the value of the inverse of that matrix. 

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


## This function calculates the inverse of a matrix, but first checks to see if (and retrieves) 
## the matrix has already been calculated. If the matrix has already been calculated (and cached), 
## then this function simply retrieves the matrix, avoiding a computation step. If the matrix has not been
## calculated, then the function calculates and returns the matrix inverse.


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


