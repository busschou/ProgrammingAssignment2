## The idea is to reuse already calculated inverse matrices and
## only do a recalculation if the original matrix has changed

## makeCacheMatrix takes a square invertible matrix as input.
## it returns a vector containing functions to get and set both the 
## matrix and the inverse, x is the matrix and xi is the inverse

makeCacheMatrix <- function(x = matrix()) {
        xi <- NULL
        setmatrix <- function(y) {
                x <<- y
                xi <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) xi <<- inverse
        getinverse <- function() xi
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolce retrieves the inverse matrix xi of matrix x from the cache 
## xi has not been cached or x has changed then inverse is calculated and 
## stored in the cache for later use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xi <- x$getinverse()
        if(!is.null(xi)) {
                message("getting cached data")
                return(xi)
        }
        data <- x$getmatrix()
        xi <- solve(data)
        x$setinverse(xi)
        xi
}
