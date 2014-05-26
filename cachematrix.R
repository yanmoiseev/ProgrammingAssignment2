#Given function calculates the inverse of matrix and store tham in cache. If the same inverse of matrix is calculated later,
# it takes the values from the cache rather than calculating them again

## This function stores a matrix in cache.

makeCacheMatrix <- function(x = matrix()) {

        inverse <- Null
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)


## This function checks if matrix is stored in cache and if not, it calculates inverse as normal. 

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
    ## Return a matrix that is the inverse of 'x'
}
