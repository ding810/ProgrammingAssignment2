## the makeCacheMatrix function and cacheSolve function serve to
## cache the inverse of a matrix, to speed up calculations

## makeCacheMatrix: creates a "special matrix", a list containing 4 functions, 2 setters
## for the matrix/its inverse, and 2 getters for the matrix/its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: takes in a "special matrix" produced by the makeCacheMatrix function, and
## gets the inverse of the matrix if it's already been calculated. If not, it calculates
## the inverse and sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
