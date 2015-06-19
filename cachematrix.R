## Yamila Omar, Programming Assignment 2, R Programming, Coursera
## Functions to chache the inverse of a matrix and only calculate it if it has
## not been previously calculated.

## This first function is a list of four functions: set, get, setinv_matrix and
## getinv_matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinv_matrix <- function(solve) inv_matrix <<- solve
        getinv_matrix <- function() inv_matrix
        list(set = set, get = get,
             setinv_matrix = setinv_matrix,
             getinv_matrix = getinv_matrix)
}


## This second function gets the value of inv_matrix and if it's NULL it 
## calculates it. If not, it gets it from cache and prints it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getinv_matrix()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$setinv_matrix(inv_matrix)
        inv_matrix
}
