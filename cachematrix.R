## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
		inverse_mtx <- NULL
        set <- function(y) {
                x <<- y
                inverse_mtx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse_mtx <<- inv
        getinv <- function() inverse_mtx
        list(set = set, get = get,
             set_inverse = setinv,
             get_inverse = getinv)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the set_inverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}
