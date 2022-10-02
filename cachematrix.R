## Put comments here that give an overall description of what your
## functions do

## These are a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: a function that creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function (y){
        x <<- y
        m <<- NULL  
    }
    get <- function () x
    setcache <- function(solve) m <<- solve
    getcache <- function() m
    list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## cacheSolve: a function that computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated and cached, the result is retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getcache()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setcache(m)
    m
}
