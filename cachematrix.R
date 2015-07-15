## makeCacheMatrix creates an object that includes a matrix and 
## may include a cache containing an inverse of the matrix.
## cacheSolve will take that object and get the inverse of the matirx
## and response with it and save it in the cache os if it is called again
## it can get the inverse form the cache instead of re-calculating it.
## This dose not include the logic of matching a new matrix to an existing 
## makeCacheMatrix object.

## This function creates an object that includes a matrix and may include a cache
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


## This function gets an inverse of a matrix and 
## will get the inverse from a cache if available
## It uses objects that look like the output of
## makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get.inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set.inverse(inv)
        i
}

