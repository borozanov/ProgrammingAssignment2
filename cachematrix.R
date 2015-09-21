# Similar to the makeVector(X), this function creates a container with get/set methods for matrix and its inverse
# This is used as argument 'MAT' for cacheSolve(MAT) function
makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    #the setter function for the matrix data
    set = function(y) {
        x <<- y
        i <<- NULL
    }
    #getter function for matrix data
    get = function() x
    #setter function for inverse data
    setinverse = function(inv) i <<- inv
    #getter function for inverse data
    getinverse = function() i
    #list of the arguments of the function
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# function that accepts a makeCacheMatrix object and stores its inverse in cache
# prints when values are pulled from cache
cacheSolve <- function(x, ...) {
    #pull the inverse data from argument
    i = x$getinverse()
    #if not null, already some data is cached. Returned it and notifiy user
    if(!is.null(i)) {
        message("getting cached inverse data")
        return(i)
    }
    #if null, calculate the inverse using solve() on the data provided as argument
    data = x$get()
    i = solve(data)
    #and set the data in cache before returning
    x$setinv(i)    
    i
}

