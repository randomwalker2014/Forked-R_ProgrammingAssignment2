## The following functions provide functions used for caching potentially 
## time-consuming computations.



# The first function, `makeCacheMtrix` creates a 'cacheable' matrix, which provides
# getter and setter functions for maintaining and retrieving the state of the matrix and its inverse
# in memory. Following functions are supported
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        setMatrix <<- function(mx) {
                x <<- mx
                inv <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}


# The following function calculates the inverse of the 'Cacheable' Matrix
# created with the above function. However, it first checks to see if the
# inverse has already been calculated and is available in memory. If so, it 
# retrieves the inverse from the cache and skips the expensive computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the
# inverse in the cache via the `setInverse` function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        mx <- x$getMatrix()
        inv <- solve(mx, ...)
        x$setInverse(inv)
        inv
} 
