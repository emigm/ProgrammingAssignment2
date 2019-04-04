## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
## So, below there are two functions that cache the inverse of a matrix.

## Create a special matrix object that cam cache its inverse.
## Param x is a matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function (mt) {
        x <<- mt
        inverse <<- NULL
    }

    get <- function () x

    setinverse <- function (inv) inverse <<- inv

    getinverse <- function () inverse

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the matrix x.
## If the inverse has already been calculated and the matrix x has not changed, then the inverse from the cache is
## returned. If not, the inverse of the matrix x is calculated, cached and then returned.
## Param x is a special matrix returned by makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inverse <- x$getinverse()

    if (is.null(inverse)) {
        mt <- x$get()
        inverse <- solve(mt)
        x$setinverse(inverse)
    }

    inverse
}
