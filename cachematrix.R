## Functions to create a matrix that caches its inverse using lexical scope
##
## makeCacheMatrix accepts a matrix as input and returns a list object that can
## store the matrix and a cached copy of its inverse.
##
## Usage:
##  - Create a cacheable matrix:
##    cacheMatrix <- makeCacheMatrix(matrix(c(4, 2, 7, 6), nrow=2, ncol=2))
##
##  - Use set_matrix to change the definition of the matrix:
##    cacheMatrix$set_matrix(matrix(c(5, 9, 1, 3), nrow=2, ncol=2))
##
##  - Use get_matrix to return current value of matrix:
##    cacheMatrix$get_matrix()
##          [,1] [,2]
##    [1,]    4    7
##    [2,]    2    6
##
##  - Use get_inverse to return current value of inverse
##    cacheMatrix$get_inverse()
##          [,1] [,2]
##    [1,]  0.6 -0.7
##    [2,] -0.2  0.4
##
##  - Use set_inverse to set the value of the cached inverse
##    inv <- solve(matrix(c(4, 2, 7, 6), nrow=2, ncol=2))
##    cacheMatrix$set_inverse(inv)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set_matrix <- function(new_x) {
        x <<- new_x
        inverse <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    list(set_matrix = set_matrix, get_matrix = get_matrix,
      set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve accepts a matrix constructed by makeCacheMatrix as input and
## returns its inverse. If the input matrix has a cached inverse, the cached
## copy is returned, otherwise the inverse is computed and stored in the
## matrix's cache.
##
## Usage:
##  - Call cacheSolve on cacheMatrix to compute inverse or retrieve from cache:
##    inverse <- cacheSolve(cacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
            message("getting cached inverse")
            return(inverse)
        }
        matrix <- x$get_matrix()
        inverse <- solve(matrix, ...)
        x$set_inverse(inverse)
        inverse
}
