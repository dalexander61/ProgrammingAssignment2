## Programming Assignment 2, Week 3 - Doug Alexander
## This function is a computationally less intensive method to calculate and 
## utilize the Inverse of a Matrix.  Since this is computationally intensive, these
## functions allow you to store the result in cache so it doesn't have to be recomputed each time.

## makeCacheMatrix() creates a list of functions to set and get the value of a Matrix and
## to set and get the inverse of the matrix and stores the values in a cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve() calculates the inverse of a matrix created with the above function.
## it first checks to see if the inverse has already been calculated.  if so, it grabs it
## from the cache.

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
