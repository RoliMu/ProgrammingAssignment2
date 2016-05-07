## Programming Assignment 2: Lexical Scoping
## 07/05(2016)
## Starting with:'Matrix inversion is usually a costly 
## computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly'
## The assignment is to write a pair of functions that cache 
## the inverse of a matrix.

## the first function makeCacheMatrix is to create a special 
## matrix object that can cache its inverse.

## the second function cacheSolve is to compute the inverse of
## the special matrix returned by makeCacheMatrix above. if the
## inverse has already been calculated (and the matrix has not 
## changed), then this second function cacheSolve should retrieve 
## the inverse from the cache.

## solve() function computes the inverse of a square matrix (if 
## invertible).

## for this assignment it is assumed that the input matrix is 
## always convertible!

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}
## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.
cacheSolve <- function (x=matrix(), ...) {
        # Need to compare matrix to what was there before!
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}