## Put comments here that give an overall description of what your
## functions do
#
# This file contains two functions, makeCacheMatrix and cacheSolve, that provide
# a matrix variable that caches the inverse after computation for as long as the
# matrix remains unchanged.#


## Write a short comment describing this function
# makeCacheMatrix(x)
#
# makeCacheMatrix creates a cachematrix entity, that is, a list containing
# four functions, setMatrix, getMatrix, setInverse, getInverse. These provide
# the ability to set, retrieve, and invert a matrix, but with caching in order
# to speed inversion when the matrix has not changed since the last inversion.
#
# Arguments:
# x     A square matrix
# 
# Methods:
# set   sets the matrix of the cachematrix list. Sets internally a flag that
#       will require recomputation if the inverse is requested later.
#
# get   returns the matrix of the cachematrix list
#
# setInverse   sets the inverse of the cachematrix list; sets internally a
#              flag that indicates that the matrix inverse has been computed
#              since the last setMatrix operation
#
# getInverse   returns the inverse matrix of the matrix represented by the
#              cachematrix list
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv

    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}

## Write a short comment describing this function
# cacheSolve(x, ...)
# 
# cacheSolve takes a cachematrix list and returns the inverse of the matrix
# associated to it, using the cached inverse when appropriate.
#
# Arguments
# x     A cachematrix list.
#
# Returns
# y     The multiplicative inverse of x.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("Getting cached data")
        return(inverse)
    }
    mat = x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
