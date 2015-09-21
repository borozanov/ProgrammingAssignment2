# Similar to the makeVector(X), this function creates a container with get/set methods for matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set = function(y) {
        x <<- y
        i <<- NULL
    }
    get = function() x
    setinverse = function(inv) i <<- inv
    getinverse = function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# function that accepts a makeCacheMatrix object and stores its inverse in cache
# prints when values are pulled from cache
cacheSolve <- function(x, ...) {
    i = x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data = x$get()
    #uses the solve() function to obtain inverse
    i = solve(data)
    x$setinv(i)
    i
}

