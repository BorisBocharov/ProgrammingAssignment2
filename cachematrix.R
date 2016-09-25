# The below two functions - makeCacheMatrix and cacheSolve - work in tandem to allow the use to save 
# potentially time-consuming computations when repeatedly calculating the inverse of an input matrix.
# Instead of calculating the inverse from scratch at every iteration, 
# it is first looked up in the cache.

# This function takes an invertible matrix as its input and creates 
# a special "matrix" object (more precisely, a list) that can cache its inverse.
# The list contains a collection of user-defined functions that allow one to manipulate both
# the original input, as well as the latter's inverse. Specifically:
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse
# 4.get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve retrieves the inverse from the cache, otherwise the inverse 
# is calculated from scratch.
cacheSolve <- function(x, ...) {
        # Fetch the inverse.
        inv <- x$getinverse()
        # If previously calculated, return the inverse and we are done.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Otherwise, recover the original matrix.
        data <- x$get()
        # Calculate its inverse.
        inv <- solve(data, ...)
        # Update the cache.
        x$setinverse(inv)
        inv
}
