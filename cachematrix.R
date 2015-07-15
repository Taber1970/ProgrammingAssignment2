## Put comments here that give an overall description of what your
## functions do

## This function gets a object that includes a matrix and may include a cache
## containing an inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set.inverse <- function(inverse) i <<- inverse
        get.inverse <- function() i
        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## This function get an inverse of a matrix and 
## will get the inverse from a cache if available
## It uses objects that look like the output of
## makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get.inverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set.inverse(i)
        i
}
