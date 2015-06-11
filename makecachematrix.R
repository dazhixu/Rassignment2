## makeCacheMatrix will do the following thing:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverMatrix) inv <<- inverMatrix
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve gives the inverse of the matrix x: if the inverse
## is in the cache, the inverse will be directly read out from the
## cache if it is already in there, otherwise, the inverse will be 
## calulated and then stored into the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}