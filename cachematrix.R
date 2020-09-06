## This function creates a special "matrix" object that can cache its inverse
##I set x as a matrix
## then set "inv" as null
## set the value of matrix and get the value of the matrix
## set the value of the Inverse and get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The following function calculates the inverse of the special 'matrix' 
##created with the above makeCacheMatrix. However, it first checks to see 
##if the inverse has already been calculated. If so, it gets the inverse 
##from the cache and skips the computation. Otherwise, it calculates 
##the inverse of the data and sets the value of the inverse in the cache via 
##the setInverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting inversed matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
