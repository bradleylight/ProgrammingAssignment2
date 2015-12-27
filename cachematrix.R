## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix(X) is a function to create an R object that caches
## a matrix and it's inverse, once that inverse has been calculated
##
## cacheSolve(X, ...) is a function to receive an R object created by
## the first function, read the cached solution/inverse if it exists,
## otherwise solve the matrix (i.e., calculate it's inverse) and cache
## the inverse for the next time this function is called on the same object

## Write a short comment describing this function
##
## makeCacheMatrix(X) takes a matrix as argument, assumed to be invertible (!)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invers = matrix()) inv <<- invers
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
##
## cacheSolve(X) takes an object created by makeCacheMatrix() as it's argument
## and calls the 'getinv', 'get' and 'setinv' methods of that object
## to get the matrix and its inverse from cached values and, if the inverse
## is not available, to calculate the inverse and cache it for use in future
## function calls

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
