## This code contains two functions that are used to create
## a special object that stores a square matrix and cache's its inverse

## !!! be careful !!!
## The input matrix should be square, reversible and well-conditioned.


## This function creates a special "matrix" object that can cache
## its inverse.
## This special "matrix" is actually a list containing a function to:
##  1. set() == set the value of the matrix
##  2. get() == get the value of the matrix
##  3. set_inv() == set the value of the inverse matrix
##  4. get_inv() == get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_matr <- NULL
    set <- function(y) {
        x <<- y
        inv_matr <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) inv_matr <<- inv
    get_inv <- function() inv_matr
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix() above. If the inverse has
## already been calculated (and the matrix has not changed),
## then the cachesolve() should retrieve the inverse from the cache

## !!! warning !!!
## There are no ellipses "..." in cacheSolve() and solve() functions
## because of confusion: function solve() has arguments solve(a, b, tol, ...)
## and has different behavior in case of solve(a) == return inverse matrix
## and solve(a, b) == return the solution of the equation a %*% x = b for x
## (see ?solve)

cacheSolve <- function(x) {
    inv_matr <- x$get_inv()
    if(!is.null(inv_matr)) {
        message("getting cached data")
        return(inv_matr)
    }
    data <- x$get()
    inv_matr <- solve(data)
    x$set_inv(inv_matr)
    inv_matr
}
