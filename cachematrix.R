## Takes a square matrix as argument and returns a list with functions
## to solve the inverse and cache it to return it if was already called once;
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
		x <<- y
		m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Takes a CacheMatrix list as argument and invokes the implemented
## getInverse, which returns the inverted matrix, cached if it was already solved
## called once or solves it and caches it for the next call;
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
