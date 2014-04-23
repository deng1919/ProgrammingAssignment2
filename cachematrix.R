## The purpose of these functions is to read a matrix and store
## the matrix and the function for solving its inverse matrix in the 
## same environment, so that when the same matrix is encountered later, 
## instead of calculating again, the value of its inverse matrix can be 
## obtained from the cache environment.

## the makeCacheMatrix is to read in a matrix, and assign several methods (get, 
## set, getsolve, setsolve) the matrix in the same environment.  The 
## function get is to get the matrix from cache if it is encountered before 
## and the getsolve function is to solve the inverse matrix for the matrix in cache.  
## The function set is to put the matrix into the same environment with its methods.
## setsolve is the function of solving inverse matrix for the matrix just set 
## to the environment (ie first encounter).

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



## This is the function used to judge if the matrix has already been there 
## in the cache. If true, it is calling
## the getsolve function to solve and return the inverse matrix and a message
## 'getting cached data'. If not, the program is calling set function
## to put it in a cache environment and solve the inverse matrix and r
## eturn the inverse matrix

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
