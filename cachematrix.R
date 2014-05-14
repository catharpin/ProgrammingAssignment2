## These functions are designed to create objects that contain both a matrix
## and its inverse, so that the inverse can be stored with the matrix, removing
## the need to calculate it repeatedly.

## makeCacheMatrix creates an list containing four functions:
## set, which sets the underlying matrix and clears the cached inverse
## get, which returns the underlying matrix
## setinverse, which caches the inverse
## getinverse, which returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(y) inverse <<- y
    getinverse <- function() inverse
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve takes a cacheable matrix object as its argument.  
## If the inverse of the matrix is cached, that value is returned; 
## otherwise this function will calculate the inverse of the underlying
## matrix, store the inverse in the passed cacheable matrix object, 
## and return the inverse.

cacheSolve <- function(x, ...) {
    cachedinverse <- x$getinverse()
    if (!is.null(cachedinverse)){
        message ("returning cached inverse")
        return(cachedinverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinverse(inverse)
    inverse
}

## Following is a test script.

testmatrix <- matrix(c(1,1,1,3,4,3,3,3,4),3,3)
print(testmatrix)

inversetestmatrix <- solve(testmatrix)
print (inversetestmatrix)

cachematrix <- makeCacheMatrix(testmatrix)

message("First solution - no cache present")
print(cacheSolve(cachematrix))

message("Second solution - should draw from the cache")
print(cacheSolve(cachematrix))


