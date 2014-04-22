## The purpose of these functions is to read a matrix and store
## the matrix and its inverse matrix in the same environment, so
## that when the same matrix is encountered later, instead of
## calculating again, the value of its inverse matrix can be obtained
## from its environment.

## the makeCacheMatrix is to read in a matrix, and put the matrix
## in the same environment of the function 'solve' to get its inverse
## matrix. get is fthe function or store the input matrix, set is the
## function for updating matrix. getsolve is the function for getting
## inverse matrix in for the get matrix. setsolve is the function for
## getting inverse matrix for the updatad matrix stored in set.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}



## This is the function used to judge if the matrix to be solved
## has already been there in the cache. If true, it is calling
## the getsolve function and return the inverse matrix and a message
## 'getting cached data'. If not, the program is calling get function
## to read in data and solve the inverse matrix, store that function
## in cache and return the inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}