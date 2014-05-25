## makeCacheMatrix stores (set) and gets the values of a matrix and its 
## inverse. It then exposes a set of functions that allows users to
##   1) set the value of the matrix (set)
##   2) get the value of the matrix (get)
##   3) set the inverse of the matrix (setinverse)
##   4) get the inverse of the matrix (getinverse)
##
## Assumptions:
##   a) inverse of the matrix will be calculated using cacheSolve function
##

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will calculate the inverse of a matrix stored using makeCacheMatrix
## function and return the value
##   1) function will first attempt to check if inverse has been calculated
##      and stored. If so, it will return the cached result.
##   2) Otherwise the function will calculate the inverse and store it using makeCacheMatrix
##
## Assumptions:
##   a) It is assumed that the matrix will always be invertible
##   b) Input 'x' is created using makeCacheMatrix
##   

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
