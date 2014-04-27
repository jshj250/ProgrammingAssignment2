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
        # get the inverse of the matrix
        inv <- x$getinv()
        # If there exists the inverse of the matrix, return this inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # get the matrix
        data <- x$get()
        # solve(X) returns the inverse of the matrix
        inv <- solve(data, ...)
        # set the inverse of the matrix
        x$setinv(inv)
        inv
}
