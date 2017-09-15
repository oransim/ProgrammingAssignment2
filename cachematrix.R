## Put comments here that give an overall description of what your
## functions do

# first function create a list object containing a matrix

## Write a short comment describing this function

# this function creates a list containing 4 functions the eventually store the cache inverse matrix
makeCacheMatrix <- function(x = matrix()) {
                    m <- NULL
                    set <- function(y) {
                        x <<- y
                        m <<- NULL
                    }
                    get <- function() x
                    setinverse <- function(matrix) m <<- matrix
                    getinverse <- function() m
                    list(set = set, get = get,
                         setinverse = setinverse,
                         getinverse = getinverse)
}


# this function recives the list object created in the function above and creates/get the stored the inverse matrix

cacheSolve <- function(x, ...) {
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
