#Coursera ProgrammingAssignment2

# A pair of functions that cache the inverse of a matrix.


## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) m <<- inverse_matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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


# Test data and code to check assignment
testm <- matrix(runif(9), 3, 3)
a <- makeCacheMatrix(testm)
cacheSolve(a)
cacheSolve(a)