## Matrix inversion is a costly computation therefore to prevent computing its inverse each
## each time it is invoked, it is considered best practise to save its cache

## makeCacheMatrix is the function that is creating the cache to be used as described above
## This function essentially creates a list of function to set and get the value of a matrix and its inverse

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

## This function checks if there is an inverse available in cache
## If yes, then it returns the cache, else calculates the inverse
## using the solve() function and sets it as cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
