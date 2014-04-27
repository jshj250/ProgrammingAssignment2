## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set the matrix
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        # get the matrix
        get <- function() x
        # set the inverse of the matrix
        setinv <- function(inversion) inv <<- inversion
        # get the inverse of the matrix
        getinv <- function() inv
        # return a list containing four functions 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
