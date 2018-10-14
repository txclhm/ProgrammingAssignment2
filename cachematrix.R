## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## x is a matrix 
## use makeCacheMatrix to cache the inverse of x
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    } 
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




## Write a short comment describing this function
## cachesolve computes the inverse x 
## if the inverse has been calculated, cachesolve retrieve inverse from the cache
cachesolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cachesolve(B1)
