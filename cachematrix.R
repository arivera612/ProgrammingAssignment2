## Two functions will cache the inverse of a maatrix
## makeCacheMatrix is a function that creates an object of class "matrix" that cache the inverse of an input

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## cacheSolve  computes the inverse of a the matrix returned by makeCacheMatrix. 
## cachesolve will retrieve the inverse from the cache if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
